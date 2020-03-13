package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

class BRU extends Module {
  val io = IO(new ISU_BRU_IO)

  /* branch check */
  val pc = io.isu.fu_in.wb.pc
  val simm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val Ia = (pc + 4.U + (simm << 2))(31, 0)
  val Ja = Cat(Seq(pc(31, 28), instr.addr, 0.U(2.W)))
  val Za = pc + (instr.imm << 2).asTypeOf(SInt(32.W)).asUInt + 4.U
  val JRa = io.rfio.rs_data.bits
  /* br_info={33:jump, 32:wb, 31..0:target} */
  val br_info = MuxLookup(0.U, io.isu.fu_in.ops.fu_op, Array(
    BR_EQ    -> Cat(s_data === rt_data, N, Ia),
    BR_NE    -> Cat(rs_data =/= rt_data, N, Ia),
    BR_LEZ   -> Cat(rs_data.asSInt <= 0.S, N, Ia),
    BR_GEZ   -> Cat(rs_data.asSInt >= 0.S, N, Ia),
    BR_LTZ   -> Cat(rs_data.asSInt < 0.S, N, Ia),
    BR_GTZ   -> Cat(rs_data.asSInt > 0.S, N, Ia),
    BR_GEZAL -> Cat(rs_data.asSInt >= 0.S, Y, Za),
    BR_LTZAL -> Cat(rs_data.asSInt < 0.S, Y, Za),
    BR_J     -> Cat(Y, Y, N, Ja),
    BR_JAL   -> Cat(Y, Y, Y, Ja),
    BR_JR    -> Cat(Y, Y, N, JRa),
    BR_JALR  -> Cat(Y, Y, Y, JRa)))

  io.fu_out.wb := io.fu_in.wb
  io.fu_out.wb.v := br_info(32)
  io.fu_out.wb.wen := br_info(32)
  io.fu_out.wb.data := pc + 8.U
  io.fu_out.wb.is_br := Y
  io.fu_out.br_target := br_info(31, 0)
}
