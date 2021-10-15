package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

import woop.utils._

class BRU extends Module {
  val io = IO(new ISU_BRU_IO {
    val can_log_now = Input(Bool())
  })

  /* branch check */
  val pc = io.fu_in.bits.wb.pc
  val instr = io.fu_in.bits.wb.instr
  val simm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt

  val op1 = io.fu_in.bits.ops.op1
  val op2 = io.fu_in.bits.ops.op2
  val Ia = (pc + 4.U + (simm << 2))(31, 0)
  val Ja = Cat(Seq(pc(31, 28), instr.addr, 0.U(2.W)))
  val Za = pc + (instr.imm << 2).asTypeOf(SInt(32.W)).asUInt + 4.U
  val JRa = op1
  /* br_info={33:jump, 32:wb, 31..0:target} */
  val br_info = MuxLookup(io.fu_in.bits.ops.fu_op, 0.U, Array(
    BR_EQ    -> Cat(op1 === op2, N, Ia),
    BR_NE    -> Cat(op1 =/= op2, N, Ia),
    BR_LEZ   -> Cat(op1.asSInt <= 0.S, N, Ia),
    BR_GEZ   -> Cat(op1.asSInt >= 0.S, N, Ia),
    BR_LTZ   -> Cat(op1.asSInt < 0.S, N, Ia),
    BR_GTZ   -> Cat(op1.asSInt > 0.S, N, Ia),
    BR_GEZAL -> Cat(op1.asSInt >= 0.S, Y, Za),
    BR_LTZAL -> Cat(op1.asSInt < 0.S, Y, Za),
    BR_J     -> Cat(Y, Y, N, Ja),
    BR_JAL   -> Cat(Y, Y, Y, Ja),
    BR_JR    -> Cat(Y, Y, N, JRa),
    BR_JALR  -> Cat(Y, Y, Y, JRa)))

  io.fu_out.valid := io.fu_in.valid && br_info(33)
  io.fu_out.bits.wb := io.fu_in.bits.wb
  io.fu_out.bits.wb.v := br_info(32)
  io.fu_out.bits.wb.wen := br_info(32)
  io.fu_out.bits.wb.data := pc + 8.U
  io.fu_out.bits.wb.is_br := Y
  io.fu_out.bits.br_target := br_info(31, 0)

  if (conf.log_BRU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "BRU")
    printv(io.fu_in, "BRU.io.fu_in")
    printv(io.fu_out, "BRU.io.fu_out")
  }
}
