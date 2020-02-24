package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class IFUPipelineData[T<:Data](gen:T, entries:Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val is_full = RegInit(N)
  val is_empty = RegInit(Y)
  val queue = Mem(entries, gen)
  def next(v:UInt) = Mux(v + 1.U === entries.U, 0.U, v + 1.U)
  val next_head = next(head)
  val next_tail = next(tail)

  io.enq.ready := !is_full || io.deq.ready
  io.deq.valid := !is_empty
  io.deq.bits := queue(tail)

  when (io.ex_flush.valid) {
    head := 0.U
    tail := 0.U
    is_full := N
    is_empty := Y
  } .elsewhen (io.br_flush.valid) {
    when (io.deq.fire()) {
      head := 0.U
      tail := 0.U
      is_full := N
      is_empty := Y
    } .otherwise {
      head := next_tail
      is_full := N
      is_empty := N
    }
  } .otherwise {
    when (io.enq.fire()) {
      queue(head) := io.enq.bits
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
    io.imem.req.fire() -> (pc + 4.U)))

  /* stage 1: synchronize */
  io.iaddr.req.valid := Y
  io.iaddr.req.bits.func := MX_RD
  io.iaddr.req.bits.vaddr := pc

  /* stage 2: blocking */
  val s2_in = RegEnable(next=pc, enable=io.iaddr.req.fire())
  val s2_data_tl = RegInit(~(0.U(log2Ceil(conf.mio_cycles + 1).W)))
  val s2_datas = Mem(conf.mio_cycles, UInt(33.W))
  val s2_out = s2_datas(s2_data_tl)
  when (io.ex_flush.valid || io.br_flush.valid) {
    for (i <- 0 until conf.mio_cycles) {
      s2_datas(i) := 0.U
    }
  } .elsewhen (io.imem.req.fire()) {
    when (io.br_flush.valid) {
    } .otherwise {
      for (i <- 1 until conf.mio_cycles) {
        s2_datas(i) := s2_datas(i - 1)
      }
      s2_datas(0) := Cat(Y, s2_in)
    }
  }
  s2_data_tl := s2_data_tl + io.imem.req.fire() - io.imem.resp.fire()
  io.imem.req.valid := io.iaddr.resp.valid
  io.imem.req.bits.is_cached := io.iaddr.resp.bits.is_cached
  io.imem.req.bits.is_aligned := Y
  io.imem.req.bits.addr  := io.iaddr.resp.bits.paddr
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.data  := 0.U
  io.imem.resp.ready := io.fu_out.ready

  /* stage 3: blocking */
  io.fu_out.valid := io.imem.resp.valid && s2_out(32)
  io.fu_out.bits.pc := s2_out(31, 0)
  io.fu_out.bits.instr := io.imem.resp.bits.data
  io.fu_out.bits.ex := 0.U.asTypeOf(new CP0Exception)

  if (conf.log_IFU) {
    val p = Seq[Bits](GTimer(), pc, s2_data_tl)
    val q = for (i <- 0 until conf.mio_cycles) yield s2_datas(i)
    printf("%d: IFU: pc=%x, s2_data_tl=%d, s2_datas={"+List.fill(conf.mio_cycles)("%x,").mkString+"}\n", (p++q):_*)
    io.imem.dump("IFU.imem")
    io.iaddr.dump("IFU.iaddr")
    io.fu_out.dump("IFU.fu_out")
    io.br_flush.dump("IFU.br_flush")
    io.ex_flush.dump("IFU.ex_flush")
  }
}

