package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

class MDUOp extends Bundle
{
  val mf_reg = UInt(1.W)
  val func   = UInt(2.W)
  val sign   = UInt(1.W)
  val wb_reg = UInt(1.W)

  def isSigned() = sign =/= 0.U
  def isDiv() = func === F_DIV
  def isMul() = func === F_MUL
  def isMove() = func === F_MV
  def isWB_HL() = wb_reg === WB_HL
  def isWB_RD() = wb_reg === WB_RD
}

class MDU extends Module with UnitOpConstants {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val fu_out = DecoupledIO(new PRALU_OUT)
  })

  io.fu_in.ready := Y

  io.fu_out.valid := N
  io.fu_out.bits := 0.U.asTypeOf(io.fu_out.bits)
}
