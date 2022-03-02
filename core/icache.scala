package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

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

class ICacheModuleIO extends Bundle {
  val in = Flipped(new MemIO)
  val out = new AXI4IO(conf.xprlen)
  val br_flush = Input(Bool())
  val ex_flush = Input(Bool())
  val can_log_now = Input(Bool())
}

class ICache extends Module {
  val io = IO(new ICacheModuleIO)

  require(isPow2(conf.nICacheSets))
  require(isPow2(conf.nICacheWays))
  require(isPow2(conf.nICacheWayBytes))

  val vbits = Module(new SeqMemAccessor(conf.nICacheSets, Vec(conf.nICacheWays, Bool())))
  val tags = Module(new SeqMemAccessor(conf.nICacheSets, Vec(conf.nICacheWays, UInt(ICacheParams.tag_bits.W))))
  val datas = Module(new SeqMemAccessor(conf.nICacheSets * conf.nICacheWays, Vec(conf.nICacheWayBytes / 4, UInt(32.W))))

  /* stage 0 */
  val s0_in = io.in.req.bits
  val s0_addr = s0_in.asTypeOf(new ICacheAddr)
  vbits.io.raddr := s0_addr.tag
  tags.io.raddr := s0_addr.tag

  // s1_out_valid = s1_valid
  // s1_out_ready = s2_match
  val s1_valid = RegInit(N)
  val s1_addr = RegEnable(next=s0_addr,
    enable=io.in.req.fire(), init=0.U.asTypeOf(s0_addr))
  val s1_matches = Reverse(Cat(for (i <- 0 until conf.nICacheWays) yield (vbits.io.rdata(i) && tags.io.rdata(i) === s1_addr.tag)))
  val s1_match = s1_matches.orR
  datas.io.raddr := Mux1H(for (i <- 0 until conf.nICacheWays)
    yield s1_matches(i) -> ((s1_addr.setno << log2Ceil(conf.nICacheWays)) + i.U))
  when (io.ex_flush || (!io.in.req.fire() && s1_match)) {
    s1_valid := N
  } .elsewhen (!io.ex_flush && io.in.req.fire()) {
    s1_valid := Y
  }
  io.in.req.ready := s1_match || !s1_valid

  // s2_in_valid = s2_valid
  // s2_in_ready = s2_match
  val s2_valid = RegInit(N)
  val s2_match = RegNext(s1_match)
  val s2_addr = RegEnable(s1_addr, enable=s1_valid && s2_match, init=0.U.asTypeOf(s1_addr))
  when (s1_match) {
  }
}

class SimICacheEntry extends Bundle {
  val v = Bool()
  val tag = UInt((32 - log2Ceil(conf.nSimICacheEntries)).W)
  val data = UInt(conf.xprlen.W)
}

class SimICacheModuleIO extends Bundle {
  val in = Flipped(new MemIO)
  val out = new MemIO
  val br_flush = Input(Bool())
  val ex_flush = Input(Bool())
  val control = Flipped(ValidIO(new CacheControl))
  val can_log_now = Input(Bool())
}

class SimICache extends Module {
  val io = IO(new SimICacheModuleIO)

  val nEntryBits = log2Ceil(conf.nSimICacheEntries)
  def idxOf(addr:UInt) = addr(nEntryBits - 1, 0)
  def tagOf(addr:UInt) = addr(31, nEntryBits)
  val cache = Mem(conf.nSimICacheEntries, new SimICacheEntry)

  when (io.control.valid && !io.ex_flush) {
    val idx = io.control.bits.addr(log2Ceil(conf.nICacheSets) - 1, 0)
    val n = conf.nSimICacheEntries / conf.nICacheSets
    require(n > 0)
    when (io.control.bits.op === I_INDEX_INVALIDATE ||
      io.control.bits.op === I_HIT_INVALIDATE) {
      for (i <- 0 until n) {
        cache(Cat(i.U, idx)).v := N
      }
    }
  }
  
  when (reset.toBool) {
    for (i <- 0 until conf.nSimICacheEntries) {
      cache(i).v := N
    }
  }

  val flush = io.br_flush || io.ex_flush

  val s0_valid = RegInit(N)
  val s0_in = RegEnable(next=io.in.req.bits, enable=io.in.req.fire(), init=0.U.asTypeOf(io.in.req.bits))
  val s0_out_ready = WireInit(N)
  val s0_out_fire = s0_valid && s0_out_ready
  io.in.req.ready := s0_out_ready || !s0_valid
  when (flush || (!io.in.req.fire() && s0_out_fire)) {
    s0_valid := N
  } .elsewhen(!flush && io.in.req.fire()) {
    s0_valid := Y
  }

  val s1_in_fire = s0_out_fire
  val s1_valid = RegInit(N)
  val s1_in = RegEnable(next=s0_in, enable=s1_in_fire, init=0.U.asTypeOf(s0_in))
  val s1_tag = tagOf(s1_in.addr)
  val s1_entry = cache(idxOf(s1_in.addr))
  val s1_hit = s1_tag === s1_entry.tag && s1_entry.v
  val s1_req = RegInit(N)
  val s1_resp = RegInit(N)
  val s1_ex_wait_en = io.ex_flush && (s1_req || s1_resp)
  val s1_ex_wait = RegEnable(next=io.ex_flush, enable=s1_ex_wait_en, init=N)
  val s1_ex_flush = (io.ex_flush && !s1_req && !s1_resp) ||
    (s1_ex_wait && io.out.resp.fire())
  s0_out_ready := (io.in.resp.ready && s1_hit) || !s1_valid
  io.in.resp.valid := s1_valid && s1_hit && !s1_ex_wait
  io.in.resp.bits.data := s1_entry.data
  when (s1_ex_flush || (!s1_in_fire && io.in.resp.fire()) ||
    (io.br_flush && io.in.resp.fire())) {
    s1_valid := N
  } .elsewhen(!s1_ex_flush && s1_in_fire) {
    s1_valid := Y
  }

  when (!io.ex_flush && s1_valid && !s1_hit && !s1_resp) { s1_req := Y }
  when (s1_req && io.out.req.fire()) {
    s1_req := N
    s1_resp := Y
  }
  when (s1_resp && io.out.resp.fire()) {
    s1_resp := N
    s1_entry.v := Y
    s1_entry.tag := s1_tag
    s1_entry.data := io.out.resp.bits.data
    s1_ex_wait := N
  }
  io.out.req.valid := s1_req
  io.out.req.bits := s1_in
  io.out.resp.ready := Y

  if (conf.log_SimICache) {
    when (io.can_log_now) { dump() }
  }
  def dump():Unit = {
    printv(this, "SimICache")
  }
}

class IMemCisternEntry extends Bundle {
  val req = ValidIO(new MemReq)
  val resp = ValidIO(new MemResp)
}

class IMemCisternModuleIO extends Bundle {
  val in = Flipped(new MemIO)
  val out = new MemIO
  val br_flush = Input(Bool())
  val ex_flush = Input(Bool())
  val can_log_now = Input(Bool())
}

class IMemCistern(entries:Int) extends Module {
  val io = IO(new IMemCisternModuleIO)

  val queue = Mem(entries, new IMemCisternEntry)

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val is_full = RegInit(N)
  val is_empty = RegInit(Y)
  val q_head = queue(head)
  val q_tail = queue(tail)
  def next(v:UInt) = Mux(v + 1.U === entries.U, 0.U, v + 1.U)
  val next_head = next(head)
  val next_tail = next(tail)
  val mreq_working = RegInit(N)

  io.in.req.ready := (!is_full || io.in.resp.fire()) && !mreq_working
  io.in.resp.valid := q_tail.req.valid && q_tail.resp.valid && !mreq_working
  io.in.resp.bits := q_tail.resp.bits

  /* flush signals */
  when (io.ex_flush || (io.br_flush && io.in.resp.fire())) {
    head := 0.U
    tail := 0.U
    is_full := N
    is_empty := Y
    for (i <- 0 until entries) {
      queue(i).req.valid := N
      queue(i).resp.valid := N
    }
  } .elsewhen(io.br_flush && !io.in.resp.fire()) {
    for (i <- 0 until entries) {
      when (i.U =/= tail) {
        queue(i).req.valid := N
        queue(i).resp.valid := N
      }
    }
    head := next_tail
    is_full := N
    is_empty := N
  } .otherwise {
    when (io.in.req.fire()) {
      q_head.req.valid := Y
      q_head.req.bits := io.in.req.bits
      q_head.resp.valid := N
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
  }

  /* mem req */
  val mreq_valid = RegInit(N)
  val mreq_idx = RegInit(0.U(log2Ceil(entries).W))
  when (is_full && !q_tail.resp.valid && !mreq_working) {
    mreq_valid := Y
    mreq_idx := next_tail
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
    when (mreq_idx === tail) {
      mreq_valid := N
      mreq_idx := 0.U
      mreq_working := N
    } .otherwise {
      mreq_valid := Y
      mreq_idx := next(mreq_idx)
    }
  }

  if (conf.log_IMemCistern) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "IMemCistern")

    val p = Seq[Bits](GTimer())
    val q = (for (i <- 0 until entries) yield Seq(
      queue(i).req.valid, queue(i).req.bits.addr,
      queue(i).resp.valid, queue(i).resp.bits.data
    )).reduce(_++_)
    val q_fmt_s = List.fill(entries)("(req[%b]=%x,resp[%b]=%x), ").mkString
    printf("%d: IMemCistern: queue={"+q_fmt_s+"}\n", (p++q):_*)

    printv(io.in, "IMemCistern.in")
    printv(io.out, "IMemCistern.out")
  }
}
