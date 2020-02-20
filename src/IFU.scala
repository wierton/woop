package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._
import DumpUtils._

class IFU extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val iaddr = new TLBTransaction
    val idu = DecoupledIO(new IFU_IDU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  // init to be valid, the first instruction
  val pc = RegInit(UInt(conf.xprlen.W), init=conf.start_addr)
  when (io.imem.req.fire()) { pc := pc + 4.U }

  /* stage 1: synchronize */
  io.iaddr.req.valid := Y
  io.iaddr.req.bits.func := MX_RD
  io.iaddr.req.bits.vaddr := pc

  /* stage 2: blocking */
  val mio_cycles = 23
  val s2_in = RegEnable(next=pc, enable=io.iaddr.req.fire())
  val s2_datas = Module(new Queue(UInt(32.W), mio_cycles))
  s2_datas.reset := io.flush.valid
  s2_datas.io.enq.valid := io.imem.req.fire()
  s2_datas.io.enq.bits := s2_in
  s2_datas.io.deq.ready := io.imem.resp.fire()
  assert (s2_datas.io.enq.fire() === io.imem.req.fire())
  io.imem.req.valid := io.iaddr.resp.valid
  io.imem.req.bits.is_cached := io.iaddr.resp.bits.is_cached
  io.imem.req.bits.is_aligned := Y
  io.imem.req.bits.addr  := io.iaddr.resp.bits.paddr
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.data  := 0.U
  io.imem.resp.ready := io.idu.ready

  /* stage 3: blocking */
  io.idu.valid := io.imem.resp.valid
  io.idu.bits.pc := s2_datas.io.deq.bits
  io.idu.bits.instr := io.imem.resp.bits.data

  if (true) {
    printf("%d: IFU: pc=%x, s2_datas={[%b,%b]:%x, [%b,%b]:%x}\n", GTimer(), pc, s2_datas.io.enq.valid, s2_datas.io.enq.ready, s2_datas.io.enq.bits, s2_datas.io.deq.valid, s2_datas.io.deq.ready, s2_datas.io.deq.bits)
    io.imem.dump("IFU.imem")
    io.iaddr.dump("IFU.iaddr")
    io.idu.dump("IFU.idu")
    io.flush.dump("IFU.flush")
  }
}

