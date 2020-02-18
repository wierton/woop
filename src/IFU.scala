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
  val pc_next = pc + 4.U(conf.xprlen.W)

  val req_fire = io.imem.req.fire()
  val resp_fire = io.imem.resp.fire()
  val instr = io.imem.resp.bits.data

  io.imem.req.valid := io.idu.ready && !io.flush.valid
  io.imem.req.bits.addr  := pc
  io.imem.req.bits.func  := MX_RD
  io.imem.req.bits.wstrb := 0.U
  io.imem.req.bits.data  := 0.U

  when(req_fire) {
    pc := pc_next
  }

  when(io.flush.valid) { pc := io.flush.bits.br_target; }

  io.imem.resp.ready := true.B

  io.idu.valid := io.imem.resp.valid && !io.flush.valid
  io.idu.bits.npc := pc
  io.idu.bits.instr := instr

  // print some logs
  when(io.imem.req.fire()) {
    log("[IFU] [CPC] >>>>>> %x <<<<<<\n", pc)
  }
  when(io.flush.valid) {
    log("[IFU] get flush signal, br to %x\n", io.flush.bits.br_target)
  }
  when(io.imem.req.valid || io.imem.req.ready) {
    log("[IFU] imem.req: ready=%x, valid=%x, addr=%x, pc=%x\n", io.imem.req.ready, io.imem.req.valid, io.imem.req.bits.addr, pc)
  }
  when(io.imem.resp.valid || io.imem.resp.ready) {
    log("[IFU] imem.resp: ready=%x, valid=%x, data=%x\n", io.imem.resp.ready, io.imem.resp.valid, io.imem.resp.bits.data)
  }
}

