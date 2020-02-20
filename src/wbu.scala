package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.utils._

class WBU extends Module {
  val io = IO(new Bundle {
    val alu = Flipped(DecoupledIO(new ALU_WBU_IO))
    val mdu = Flipped(DecoupledIO(new MDU_WBU_IO))
    val lsu = Flipped(DecoupledIO(new LSU_WBU_IO))
    val bru = Flipped(DecoupledIO(new BRU_WBU_IO))
    val wb = ValidIO(new WriteBackIO)
    val flush = ValidIO(new FlushIO)
  })

  val wb_pc = RegInit(0.U(conf.xprlen.W))
  val wb_addr = RegInit(0.U(conf.xprlen.W))
  val wb_data = RegInit(0.U(conf.xprlen.W))
  val wb_wen = RegInit(N)

  // valid signals
  val alu_valid = RegNext(next=io.alu.fire(), init=N)
  val mdu_valid = RegNext(next=io.mdu.fire(), init=N)
  val lsu_valid = RegNext(next=io.lsu.fire(), init=N)
  val bru_valid = RegNext(next=io.bru.fire(), init=N)

  // flush_valid
  val flush_valid = RegEnable(next=io.bru.bits.need_br, init=N, enable=io.bru.fire())

  // fu_valid
  val fu_valid = alu_valid || mdu_valid || lsu_valid || bru_valid

  assert(AtMost1H(io.alu.valid, io.mdu.valid, io.lsu.valid, io.bru.valid))

  io.alu.ready := Y
  io.mdu.ready := Y
  io.lsu.ready := Y
  io.bru.ready := Y

  // wb
  io.wb.bits.pc := wb_pc
  io.wb.bits.rd_idx := wb_addr
  io.wb.bits.data := wb_data
  io.wb.bits.wen := wb_wen
  io.wb.valid := fu_valid

  // branch
  io.flush.valid := flush_valid
  io.flush.bits.br_target := RegEnable(next=io.bru.bits.br_target, init=0.U, enable=io.bru.fire())

  when(flush_valid) { flush_valid := N }
}

