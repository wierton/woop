package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class IFU extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val iaddr = new TLBTransaction
    val idu = DecoupledIO(new IFU_IDU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  // init to be valid, the first instruction
  val pc = RegInit(UInt(conf.xprlen.W), init=conf.start_addr)
  val s1_valid = RegInit(N)
  val s2_valid = RegInit(N)

  when (io.imem.req.ready || !s1_valid) { pc := pc + 4.U }

  /* stage 1: synchronize */
  io.iaddr.req.valid := io.imem.req.ready || !s1_valid
  io.iaddr.req.bits.func := MX_RD
  io.iaddr.req.bits.vaddr := pc
  when (io.flush.valid || (!io.iaddr.req.fire() && io.imem.req.fire())) {
    s1_valid := N
  } .elsewhen(!io.flush.valid && io.iaddr.req.fire()) {
    s1_valid := Y
  }

  /* stage 2: blocking */
  io.imem.req.valid := io.iaddr.resp.valid
  io.imem.req.bits.is_aligned := Y
  io.imem.req.bits.addr  := io.iaddr.resp.bits.paddr
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.data  := 0.U
  io.imem.resp.ready := io.idu.ready
  when (io.flush.valid || (!io.imem.req.fire() && io.idu.fire())) {
    s2_valid := N
  } .elsewhen(!io.flush.valid && io.imem.req.fire()) {
    s2_valid := Y
  }

  /* stage 3: blocking */
  io.idu.valid := io.imem.resp.valid
  io.idu.bits.npc := pc
  io.idu.bits.instr := io.imem.resp.bits.data
}

