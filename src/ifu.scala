package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

/* without cache */
class IFUPipelineData[T<:Data](gen:T, entries:Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(ValidIO(gen))
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val is_full = RegInit(N)
  val is_empty = RegInit(Y)
  val queue = Mem(entries, ValidIO(gen))
  def next(v:UInt) = Mux(v + 1.U === entries.U, 0.U, v + 1.U)
  val next_head = next(head)
  val next_tail = next(tail)

  io.enq.ready := !is_full || io.deq.ready
  io.deq.valid := !is_empty
  io.deq.bits := queue(tail)

  when (io.br_flush.valid || io.ex_flush.valid) {
    val clear_all = io.ex_flush.valid || io.deq.fire()
    for (i <- 0 until entries) {
      queue(i).valid := Mux(clear_all,
        N, Mux(i.U === tail, queue(i).valid, N))
    }
  }

  when (io.enq.fire()) {
    val q_head = queue(head)
    when ((!io.br_flush.valid && !io.ex_flush.valid) || is_empty) {
      q_head.valid := Y
    }
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
  if (conf.log_IFUPipelineData) {
    printf("%d: IFUPD.io: enq[%b,%b]=%x, deq[%b,%b]=%b%x, br_flush=%b, ex_flush=%b\n", GTimer(), io.enq.valid, io.enq.ready, io.enq.bits.asUInt, io.deq.valid, io.deq.ready, io.deq.bits.valid, io.deq.bits.bits.asUInt, io.br_flush.valid, io.ex_flush.valid)
    printf("%d: IFUPD: head=%d, tail=%d, is_full=%d, is_empty=%d, next_head=%d, next_tail=%d\n", GTimer(), head, tail, is_full, is_empty, next_head, next_tail)
    val p = Seq[Bits](GTimer())
    val q = for (i <- 0 until entries) yield queue(i).asUInt
    printf("%d: IFUPD: queue={"+List.fill(entries)("%x,").mkString+"}\n", (p++q):_*)
  }
  assert (!is_full || !is_empty)
}

class IFU extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val iaddr = new TLBTransaction
    val fu_out = DecoupledIO(new IFU_BRIDU_IO)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  // init to be valid, the first instruction
  val pc = RegInit(UInt(conf.xprlen.W), init=conf.start_addr)
  pc := MuxCase(pc, Array(
    io.ex_flush.valid -> io.ex_flush.bits.br_target,
    io.br_flush.valid -> io.br_flush.bits.br_target,
    io.iaddr.req.fire() -> (pc + 4.U)))

  /* stage 1: synchronize */
  io.iaddr.req.valid := Y && !io.ex_flush.valid && !io.br_flush.valid
  io.iaddr.req.bits.func := MX_RD
  io.iaddr.req.bits.vaddr := pc
  io.iaddr.resp.ready := io.imem.req.ready

  /* stage 2: blocking */
  val s2_in = RegEnable(next=pc, enable=io.iaddr.req.fire())
  val s2_datas = Module(new IFUPipelineData(UInt(32.W), conf.mio_cycles))
  val s2_out = s2_datas.io.deq.bits
  s2_datas.io.enq.valid := io.imem.req.fire()
  s2_datas.io.enq.bits := s2_in
  s2_datas.io.deq.ready := io.imem.resp.fire()
  s2_datas.io.br_flush <> io.br_flush
  s2_datas.io.ex_flush <> io.ex_flush
  io.imem.req.valid := io.iaddr.resp.valid
  io.imem.req.bits.is_cached := io.iaddr.resp.bits.is_cached
  io.imem.req.bits.is_aligned := Y
  io.imem.req.bits.addr  := io.iaddr.resp.bits.paddr
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.data  := 0.U
  io.imem.resp.ready := io.fu_out.ready

  /* stage 3: blocking */
  io.fu_out.valid := io.imem.resp.valid && s2_out.valid
  io.fu_out.bits.pc := s2_out.bits
  io.fu_out.bits.instr := io.imem.resp.bits.data
  io.fu_out.bits.ex := 0.U.asTypeOf(new CP0Exception)

  if (conf.log_IFU) {
    printf("%d: IFU: pc=%x, s2_datas={enq[%b,%b]:%x, deq[%b,%b]:%b%x}\n", GTimer(), pc, s2_datas.io.enq.valid, s2_datas.io.enq.ready, s2_datas.io.enq.bits, s2_datas.io.deq.valid, s2_datas.io.deq.ready, s2_datas.io.deq.bits.valid, s2_datas.io.deq.bits.bits)
    io.imem.dump("IFU.imem")
    io.iaddr.dump("IFU.iaddr")
    io.fu_out.dump("IFU.fu_out")
    io.br_flush.dump("IFU.br_flush")
    io.ex_flush.dump("IFU.ex_flush")
  }
}

