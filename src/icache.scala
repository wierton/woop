package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

object ICacheParams {
  val setno_bits = log2Ceil(conf.nICacheSets)
  val off_bits = log2Ceil(conf.nICacheWayBytes / 4)
  val tag_bits = conf.xprlen - setno_bits - off_bits - 2
}

class ICacheAddr extends Bundle {
  val tag = UInt(ICacheParams.tag_bits.W)
  val setno = UInt(ICacheParams.setno_bits.W)
  val off = UInt(ICacheParams.off_bits.W)
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
    val flush = Input(Bool())
  })

  require(isPow2(conf.nICacheSets))
  require(isPow2(conf.nICacheWays))
  require(isPow2(conf.nICacheWayBytes))

  val seqmem_wen = WireInit(N)
  val vntag_array = SyncReadMem(conf.nICacheSets, Vec(conf.nICacheWays, new ICacheWayVnTag))
  val data_array = SyncReadMem(conf.nICacheSets * conf.nICacheWays, Vec(conf.nICacheWayBytes / 4, UInt(32.W)))

  val s0_in = io.in.req.bits
  val s0_addr = s0_in.asTypeOf(new ICacheAddr)

  // s1_out_valid = s1_valid
  // s1_out_ready = s2_match
  val s1_valid = RegInit(N)
  val s1_set_vntags = vntag_array.read(s0_addr.setno, !seqmem_wen)
  val s1_addr = RegEnable(s0_addr, enable=io.in.req.fire())
  val s1_match_res = Cat(for (i <- 0 until conf.nICacheWays) yield (s1_set_vntags(i).v && s1_set_vntags(i).tag === s1_addr.tag))
  val s1_match = s1_match_res.orR
  val s1_way_idx = Mux1H(for (i <- 0 until conf.nICacheWays) yield s1_match_res(i) -> i.U)
  val s1_data_array_idx = (s1_addr.setno << log2Ceil(conf.nICacheWays)) + s1_way_idx
  when (io.flush || (!io.in.req.fire() && s1_match)) {
    s1_valid := N
  } .elsewhen (!io.flush && io.in.req.fire()) {
    s1_valid := Y
  }
  io.in.req.ready := s1_match || !s1_valid

  // s2_in_valid = s2_valid
  // s2_in_ready = s2_match
  val s2_valid = RegInit(N)
  val s2_match = RegInit(N)
  val s2_addr = RegEnable(s1_addr, enable=s1_valid && s2_match)
  val s2_way_data = data_array.read(s1_data_array_idx, !seqmem_wen)
  val s2_way_res = s2_way_data(s2_addr.off)
  when (s1_valid && s2_match) { s2_match := s1_match }

  /* memory request */
  val lowWidth = s2_addr.off.getWidth + s2_addr.l2bits.getWidth
  val s2_req_valid = RegInit(N)
  val s2_req_addr = RegEnable(Cat(s2_addr.tag, s2_addr.setno, 0.U(lowWidth.W)), enable=s2_req_valid)
  val s2_req_addr_end = s2_req_addr + Cat(1.U, 0.U(lowWidth.W))
  val s2_req_addr_off = s2_req_addr.asTypeOf(new ICacheAddr).off
  io.in.resp.valid := s2_match
  io.in.resp.bits.data := s2_way_res
  when (s2_valid && !s2_match) { s2_req_valid := Y }
  when (io.out.req.fire()) {
    s2_req_addr := s2_req_addr + 4.U
  }
  when (io.out.resp.fire()) {
    // s2_way_data(s2_req_addr_off) := io.out.resp.bits
  }
  when (s2_req_addr >= s2_req_addr_end) {
    seqmem_wen := Y
    // vntag_array.write(0.U, Cat(Y, s2_addr.tag).asTypeOf(new ICacheWayVnTag), Seq(N, N, Y, N))
    data_array.write(0.U, s2_way_data)
    s2_req_valid := N
    s2_match := Y
  }
  io.out.req.valid := s2_req_valid
  io.out.req.bits.addr := s2_req_addr
  io.out.req.bits.is_cached := Y
  io.out.req.bits.is_aligned := Y
  io.out.req.bits.func := MX_RD
  io.out.req.bits.strb := 0.U
  io.out.req.bits.data := 0.U
  io.out.resp.ready := s2_req_valid
}

class ICache extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new AXI4IO(4, conf.xprlen)
    val flush = Input(Bool())
  })

  io.in <> DontCare
  io.out <> DontCare
}

class IMemCistern(entries:Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new MemIO
    val br_flush = Input(Bool())
    val ex_flush = Input(Bool())
  })

  val queue = Mem(entries, new Bundle {
    val req = ValidIO(new MemReq)
    val resp = ValidIO(new MemResp)
  })

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val is_full = RegInit(N)
  val is_empty = RegInit(Y)
  val q_head = queue(head)
  def next(v:UInt) = Mux(v + 1.U === entries.U, 0.U, v + 1.U)
  val next_head = next(head)
  val next_tail = next(tail)

  io.in.req.ready := !is_full || io.in.resp.fire()
  io.in.resp.valid := is_full && q_head.resp.valid
  io.in.resp.bits := queue(tail).resp.bits

  /* flush signals */
  when (io.br_flush || io.ex_flush) {
    val clear_all = io.ex_flush || io.in.resp.fire()
    for (i <- 0 until entries) {
      queue(i).req.valid := Mux(clear_all,
        N, Mux(i.U === tail, queue(i).req.valid, N))
      queue(i).resp.valid := Mux(clear_all,
        N, Mux(i.U === tail, queue(i).resp.valid, N))
    }
  }

  when (io.in.req.fire()) {
    when ((!io.br_flush && !io.ex_flush) || is_empty) {
      q_head.req.valid := Y
      q_head.resp.valid := N
    }
    q_head.req.bits := io.in.req.bits
    head := next_head
    is_empty := N
    when (next_head === tail && !io.in.resp.fire()) {
      is_full := Y
    }
  }

  when (io.in.resp.fire()) {
    tail := next_tail
    when (!(is_full && io.in.req.fire())) { is_full := N }
    when (next_tail === head && !io.in.req.fire()) {
      is_empty := Y
    }
  }

  /* mem req */
  val mreq_valid = RegInit(N)
  val mreq_working = RegInit(N)
  val mreq_idx = RegInit(0.U(log2Ceil(entries).W))
  when (is_full && !q_head.resp.valid && !mreq_working) {
    mreq_valid := Y
    mreq_idx := next_head
    mreq_working := Y
  }

  val q_cur = queue(mreq_idx)
  io.out.req.valid := mreq_valid
  io.out.req.bits := q_cur.req.bits
  io.out.resp.ready := Y
  when (io.out.req.fire()) {
    mreq_valid := N
  }
  when (io.out.resp.fire()) {
    q_cur.resp.valid := Y
    q_cur.resp.bits := io.out.resp.bits
    when (mreq_idx === head) {
      mreq_valid := N
      mreq_idx := 0.U
      mreq_working := N
    } .otherwise {
      mreq_valid := Y
      mreq_idx := next(mreq_idx)
    }
  }

  if (conf.log_IMemCistern) {
    when (TraceTrigger()) { dump() }
  }

  def dump():Unit = {
    val p = Seq[Bits](GTimer())
    val q = (for (i <- 0 until entries) yield Seq(
      queue(i).req.valid, queue(i).req.bits.addr,
      queue(i).resp.valid, queue(i).resp.bits.data
    )).reduce(_++_)
    val q_fmt_s = List.fill(entries)("(req[%b]=%x,resp[%b]=%x), ").mkString
    printf("%d: IMemCistern: head=%d, tail=%d, is_full=%b, is_empty=%b, mreq_valid=%b, mreq_idx=%d, working=%b, br=%b, ex=%b\n", GTimer(), head, tail, is_full, is_empty, mreq_valid, mreq_idx, mreq_working, io.br_flush, io.ex_flush)
    printf("%d: IMemCistern: queue={"+q_fmt_s+"}\n", (p++q):_*)
    io.in.dump("IMemCistern.in")
    io.out.dump("IMemCistern.out")
  }
}
