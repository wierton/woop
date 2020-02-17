package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class IFU extends Module {
  val io = IO(new Bundle {
    val mem = new MemIO
    val idu = DecoupledIO(new IFU_IDU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  // init to be valid, the first instruction
  val pc = RegInit(UInt(conf.xprlen.W), init=conf.start_addr)
  val pc_next = pc + 4.U(conf.xprlen.W)

  val req_fire = io.mem.req.fire()
  val resp_fire = io.mem.resp.fire()
  val instr = io.mem.resp.bits.data

  io.mem.req.valid := io.idu.ready && !io.flush.valid
  io.mem.req.bits.addr  := pc
  io.mem.req.bits.func  := MX_RD
  io.mem.req.bits.wstrb := 0.U
  io.mem.req.bits.data  := 0.U

  when(req_fire) {
    pc := pc_next
  }

  when(io.flush.valid) { pc := io.flush.bits.br_target; }

  io.mem.resp.ready := true.B

  io.idu.valid := io.mem.resp.valid && !io.flush.valid
  io.idu.bits.npc := pc
  io.idu.bits.instr := instr

  // print some logs
  when(io.mem.req.fire()) {
    log("[IFU] [CPC] >>>>>> %x <<<<<<\n", pc)
  }
  when(io.flush.valid) {
    log("[IFU] get flush signal, br to %x\n", io.flush.bits.br_target)
  }
  when(io.mem.req.valid || io.mem.req.ready) {
    log("[IFU] mem.req: ready=%x, valid=%x, addr=%x, pc=%x\n", io.mem.req.ready, io.mem.req.valid, io.mem.req.bits.addr, pc)
  }
  when(io.mem.resp.valid || io.mem.resp.ready) {
    log("[IFU] mem.resp: ready=%x, valid=%x, data=%x\n", io.mem.resp.ready, io.mem.resp.valid, io.mem.resp.bits.data)
  }
}

