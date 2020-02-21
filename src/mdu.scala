package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._

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
    val bypass = ValidIO(new BypassIO)
    val isu = Flipped(DecoupledIO(new ISU_MDU_IO))
    val wbu = DecoupledIO(new EXU_WBU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  val datain = RegEnable(next=io.isu.bits, enable=io.isu.fire())
  val isu_valid = RegNext(next=io.isu.fire(), init=false.B)
  val in_stage_1 = isu_valid
  val fu_op = datain.fu_op
  val op1 = datain.op1
  val op2 = datain.op2
  val rd_idx = datain.rd_idx

  val hi = RegInit(0.U(conf.xprlen.W))
  val lo = RegInit(0.U(conf.xprlen.W))

  val whi = WireInit(0.U(conf.xprlen.W))
  val wlo = WireInit(0.U(conf.xprlen.W))

  val mduop = fu_op.asTypeOf(new MDUOp)

  io.isu.ready := !in_stage_1 || io.wbu.ready

  /* bypass */
  io.bypass.valid := io.wbu.valid
  io.bypass.bits.wen := io.wbu.bits.wb.wen
  io.bypass.bits.rd_idx := io.wbu.bits.wb.rd_idx
  io.bypass.bits.data := io.wbu.bits.wb.data

  /* commit */
  io.wbu.valid := in_stage_1 && !io.flush.valid
  io.wbu.bits.wb.pc := datain.pc
  io.wbu.bits.wb.data := wlo
  io.wbu.bits.wb.wen := mduop.wb_reg === WB_RD
  io.wbu.bits.wb.rd_idx := rd_idx
  io.wbu.bits.ex := 0.U.asTypeOf(io.wbu.bits.ex)

  when(mduop.isMul()) {
    val result = Wire(UInt((2 * conf.xprlen).W))
    when(mduop.isSigned()) {
      result := (op1.asSInt + op2.asSInt).asUInt
    } .otherwise {
      result := op1.asUInt + op2.asUInt
    }

    whi := result(2 * conf.xprlen - 1, conf.xprlen)
    wlo := result(conf.xprlen - 1, 0)
  } .elsewhen(mduop.isDiv()) {
    when(mduop.isSigned()) {
      whi := (op1.asSInt + op2.asSInt).asUInt
      wlo := (op1.asSInt + op2.asSInt).asUInt
    } .otherwise {
      whi := op1.asUInt + op2.asUInt
      wlo := op1.asUInt + op2.asUInt
    }
  } .otherwise {
    when(mduop.mf_reg === MF_LO) {
      io.wbu.bits.wb.data := lo
    } .otherwise {
      io.wbu.bits.wb.data := hi
    }
  }

  when(io.isu.fire() && mduop.wb_reg === WB_HL) {
    hi := whi
    lo := wlo
  }
}
