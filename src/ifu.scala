package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._


/* without cache */
class IMemPipe[T<:Data](gen:T, entries:Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(ValidIO(gen))
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
  })

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val is_full = RegInit(N)
  val is_empty = RegInit(Y)
  val queue = Mem(entries, ValidIO(gen))
  def next(v:UInt) = Mux(v + 1.U === entries.U, 0.U, v + 1.U)
  val next_head = next(head)
  val next_tail = next(tail)
  val q_head = queue(head)
  val clear_all = io.ex_flush.valid || io.deq.fire()

  io.enq.ready := !is_full || io.deq.ready
  io.deq.valid := !is_empty
  io.deq.bits := queue(tail)

  // ?enq ?br  ex ?deq:
  // ?enq  br !ex  deq:
  //   clear all
  // ?enq  br !ex !deq:
  //   keep tail, set head to next_tail
  //  enq !br !ex  deq:

  when (io.ex_flush.valid || (io.br_flush.valid && io.deq.fire())) {
    head := 0.U
    tail := 0.U
    is_full := N
    is_empty := Y
    for (i <- 0 until entries) { queue(i).valid := N }
  } .elsewhen(io.br_flush.valid && !io.deq.fire()) {
    for (i <- 0 until entries) {
      when (i.U =/= tail) { queue(i).valid := N }
    }
    head := next_tail
    is_full := N
    is_empty := N
  } .otherwise {
    when (io.enq.fire()) {
      q_head.valid := Y
      q_head.bits := io.enq.bits
      head := next_head
      is_empty := N
      when (next_head === tail && !io.deq.fire()) {
        is_full := Y
      }
    }

    when (io.deq.fire()) {
      tail := next_tail
      when (!(is_full && io.enq.fire())) { is_full := N }
      when (next_tail === head && !io.enq.fire()) {
        is_empty := Y
      }
    }
  }

  if (conf.log_IMemPipe) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("%d: IFUPD.io: enq[%b,%b]=%x, deq[%b,%b]=%b%x, br_flush=%b, ex_flush=%b\n", GTimer(), io.enq.valid, io.enq.ready, io.enq.bits.asUInt, io.deq.valid, io.deq.ready, io.deq.bits.valid, io.deq.bits.bits.asUInt, io.br_flush.valid, io.ex_flush.valid)
    printf("%d: IFUPD: head=%d, tail=%d, is_full=%d, is_empty=%d, next_head=%d, next_tail=%d\n", GTimer(), head, tail, is_full, is_empty, next_head, next_tail)
    val p = Seq[Bits](GTimer())
    val q = for (i <- 0 until entries) yield queue(i).asUInt
    printf("%d: IFUPD: queue={"+List.fill(entries)("%x,").mkString+"}\n", (p++q):_*)
  }
  assert (!is_full || !is_empty)
}

class IMemPipeData extends Bundle {
  val ex = new CP0Exception
  val pc = UInt(conf.xprlen.W)
}

class IFU extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val iaddr = new TLBTransaction
    val fu_out = DecoupledIO(new IFU_IDU_IO)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
  })

  // init to be valid, the first instruction
  val pc = RegInit(UInt(conf.xprlen.W), init=conf.start_addr)
  val pc_misaligned = pc(1, 0) =/= 0.U
  val s0_bad_if = RegInit(N)
  pc := MuxCase(pc, Array(
    io.ex_flush.valid -> io.ex_flush.bits.br_target,
    io.br_flush.valid -> io.br_flush.bits.br_target,
    io.iaddr.req.fire() -> (pc + 4.U)))
  when (io.ex_flush.valid) {
    s0_bad_if := N
  } .elsewhen (io.iaddr.req.valid && pc(1, 0) =/= 0.U) {
    s0_bad_if := Y
  }

  /* stage 1: synchronize */
  io.iaddr.req.valid := Y && !io.ex_flush.valid && !io.br_flush.valid && !s0_bad_if
  io.iaddr.req.bits.func := MX_RD
  io.iaddr.req.bits.len := ML_4
  io.iaddr.req.bits.is_aligned  := Y
  io.iaddr.req.bits.vaddr := pc
  io.iaddr.resp.ready := io.imem.req.ready

  /* stage 2: blocking */
  val s1_in = WireInit(0.U.asTypeOf(new IMemPipeData))
  s1_in.pc := RegEnable(next=pc, enable=io.iaddr.req.fire())
  s1_in.ex := io.iaddr.resp.bits.ex

  val s1_datas = Module(new IMemPipe(new IMemPipeData, conf.icache_stages))
  val s1_out = s1_datas.io.deq.bits
  val s1_ex_in = io.iaddr.resp.valid && s1_in.ex.et =/= ET_None
  val s1_out_has_ex = s1_out.bits.ex.et =/= ET_None
  s1_datas.io.enq.valid := io.imem.req.fire() || s1_ex_in
  s1_datas.io.enq.bits := s1_in
  s1_datas.io.deq.ready := io.imem.resp.fire() || s1_out_has_ex
  s1_datas.io.br_flush <> io.br_flush
  s1_datas.io.ex_flush <> io.ex_flush
  s1_datas.io.can_log_now := io.can_log_now
  io.imem.req.valid := io.iaddr.resp.valid && !io.ex_flush.valid && s1_in.ex.et === ET_None
  io.imem.req.bits.is_cached := io.iaddr.resp.bits.is_cached
  io.imem.req.bits.is_aligned := Y
  io.imem.req.bits.addr  := io.iaddr.resp.bits.paddr
  io.imem.req.bits.len   := (conf.xprlen / 8 - 1).U
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.strb  := "b1111".U
  io.imem.req.bits.data  := 0.U
  io.imem.resp.ready := io.fu_out.ready

  /* stage 3: blocking */
  io.fu_out.valid := (io.imem.resp.valid || s1_out_has_ex) && s1_out.valid && !io.ex_flush.valid
  io.fu_out.bits.pc := s1_out.bits.pc
  io.fu_out.bits.instr := Mux(s1_out_has_ex, 0.U, io.imem.resp.bits.data)
  io.fu_out.bits.ex := s1_out.bits.ex

  if (conf.log_IFU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("%d: IFU: pc=%x, s1_datas={enq[%b,%b]:%x, deq[%b,%b]:%b%x}, s1_in={et:%d, code:%d, pc:%x}\n", GTimer(), pc, s1_datas.io.enq.valid, s1_datas.io.enq.ready, s1_datas.io.enq.bits.pc, s1_datas.io.deq.valid, s1_datas.io.deq.ready, s1_datas.io.deq.bits.valid, s1_datas.io.deq.bits.bits.pc, s1_in.ex.et, s1_in.ex.code, s1_in.pc)
    io.imem.dump("IFU.imem")
    printv(io.imem.req.bits)
    io.iaddr.dump("IFU.iaddr")
    io.fu_out.dump("IFU.fu_out")
    io.br_flush.dump("IFU.br_flush")
    io.ex_flush.dump("IFU.ex_flush")
  }
}

