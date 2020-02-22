package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

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
  when (io.ex_flush.valid) {
  } .elsewhen (io.br_flush.valid) {
    for (i <- 1 until conf.mio_cycles) {
      s2_datas(i) := 0.U
    }
    when (io.fu_out.fire()) {
      s2_datas(0) := 0.U
      s2_data_tl := ~(0.U(log2Ceil(conf.mio_cycles + 1).W))
    } .otherwise {
      s2_datas(0) := s2_out
      s2_data_tl := 0.U
    }
  } .elsewhen (io.imem.req.fire()) {
    for (i <- 1 until conf.mio_cycles) {
      s2_datas(i) := s2_datas(i - 1)
    }
    s2_datas(0) := Cat(Y, s2_in)
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
  assert (!(io.imem.req.fire() && !io.imem.resp.fire() && s2_datas(conf.mio_cycles - 1)(32)))
}

