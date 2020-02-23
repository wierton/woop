package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

object ICacheParams {
  val setno_bits = log2Ceil(conf.nICacheSets)
  val off_bits = log2Ceil(conf.nICacheWayWords)
  val tag_bits = conf.xprlen - setno_bits - off_bits - 2
}

class ICacheAddr extends Bundle {
  val tag = UInt(ICacheParams.tag_bits.W)
  val setno = UInt(ICacheParams.setno_bits)
  val off = UInt(ICacheParams.off_bits)
  val l2bits = UInt(2.W)
}

class ICacheWayVnTag extends Bundle {
  val v = Bool()
  val tag = UInt(ICacheParams.tag_bits.W)
}

class ICacheMemIO extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new MemIO
  })

  require(isPow2(conf.nICacheSets))
  require(isPow2(conf.nICacheWays))
  require(isPow2(conf.nICacheWayWords))

  val seqmem_wen = WireInit(N)
  val vntag_array = SeqMem(conf.nICacheSets, Vec(conf.nICacheWays, new ICacheWayVnTag))
  val data_array = SeqMem(conf.nICacheSets * conf.nICacheWays, Vec(conf.nICacheWayWords, UInt(32.W)))

  val s0_in = io.in.bits
  val s0_addr = s0_in.asTypeOf(new ICacheAddr)

  // s1_out_valid = s1_valid
  // s1_out_ready = s2_match
  val s1_valid = RegInit(N)
  val s1_set_vntags = vntag_array.read(s0_addr.setno, !seqmem_wen)
  val s1_addr = RegEnable(s0_addr, enable=io.in.req.fire())
  val s1_match_res = Cat(for (i <- 0 until conf.nICacheWays) yield (s1_set_vntags(i).v && s1_set_vntags(i).tag === s1_addr.tag))
  val s1_match = s1_match_res.orR
  val s1_way_idx = Mux1H(for (i <- 0 until conf.nICacheWays) yield s1_match_res(i) -> i.U)
  val s1_data_array_idx = s1_addr.setno * conf.nICacheWays + s1_way_idx
  when (io.flush.valid || (!io.in.req.fire() && s1_match)) {
    s1_valid := N
  } .elsewhen (!io.flush.valid && io.in.req.fire()) {
    s1_valid := Y
  }
  io.in.ready := s1_match || !s1_valid

  // s2_in_valid = s2_valid
  // s2_in_ready = s2_match
  val s2_valid = RegInit(N)
  val s2_match = RegInit(N)
  val s2_addr = RegEnable(s1_addr, enable=s1_valid && s2_match)
  val s2_way_data = data_array.read(s1_data_array_idx, !seqmem_wen)
  val s2_way_res = s2_way_data(s2_addr.off)
  when (s1_valid && s2_match) { s2_match := s1_match }

  val lowWidth = s2_addr.off.getWidth + s2_addr.l2bits.getWidth
  val s2_req_addr = Cat(s2_addr.tag, s2_addr.setno, 0.U(lowWidth.W))
  val s2_req_addr_end = s2_req_addr + Cat(1.U, 0.U(lowWidth.W))
  val s2_req_addr_off = s2_req_addr.asTypeOf(new ICacheAddr).off
  io.in.resp.valid := s2_match
  io.in.resp.bits.data := s2_way_res
  when (io.out.req.fire()) {
    s2_req_addr := s2_req_addr + 4.U
  }
  when (io.out.resp.fire()) {
    s2_way_data(s2_req_addr_off) := io.out.resp.bits
  }
  when (s2_req_addr >= s2_req_addr_end) {
    seqmem_wen := Y
    vntag_array.write(Cat(Y, s2_addr.tag).asTypeOf(new ICacheWayVnTag))
    data_array.write(s2_data_array_idx, s2_way_data)
    s2_req_valid := N
    s2_match := Y
  }
  io.out.req.valid := s2_req_valid
  io.out.req.bits.addr := s2_req_addr
  io.out.req.bits.is_cached := Y
  io.out.req.bits.is_aligned := Y
  io.out.req.bits.func := MX_RD
  io.out.req.bits.wstrb := 0.U
  io.out.req.bits.data := 0.U
  io.out.resp.ready := s2_req_valid
}

