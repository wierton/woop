package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._


class MDUOp extends Bundle
{
  val id     = UInt(4.W)
  val wb_reg = UInt(1.W)
}

class MDU extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new EHU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
    val can_log_now = Input(Bool())
  })

  val hi = RegInit(0.U(conf.xprlen.W))
  val lo = RegInit(0.U(conf.xprlen.W))

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)

  val fu_op = fu_in.ops.fu_op
  val op1   = fu_in.ops.op1
  val op2   = fu_in.ops.op2
  val result = Mux1H(Array(
    (fu_op === MDU_MFHI) -> hi,
    (fu_op === MDU_MFLO) -> lo,
    (fu_op === MDU_MUL)  -> (op1 * op2)))
  val wb_rd = fu_op.asTypeOf(new MDUOp).wb_reg === WB_RD
  val wb_hilo = !wb_rd
  val s_mul = (op1.asSInt * op2.asSInt).asUInt
  val u_mul = op1.asUInt * op2.asUInt
  val s_div = (op1.asSInt / op2.asSInt).asUInt
  val s_mod = (op1.asSInt % op2.asSInt).asUInt
  val u_div = op1.asUInt / op2.asUInt
  val u_mod = op1.asUInt % op2.asUInt

  when (fu_valid && wb_hilo) {
    hi := Mux1H(Array(
      (fu_op === MDU_MTHI)  -> fu_in.ops.op1,
      (fu_op === MDU_MTLO)  -> hi,
      (fu_op === MDU_MULT)  -> s_mul(63, 32),
      (fu_op === MDU_MULTU) -> u_mul(63, 32),
      (fu_op === MDU_DIV)   -> s_mod,
      (fu_op === MDU_DIVU)  -> u_mod))
    lo := Mux1H(Array(
      (fu_op === MDU_MTHI)  -> lo,
      (fu_op === MDU_MTLO)  -> fu_in.ops.op1,
      (fu_op === MDU_MULT)  -> s_mul(31, 0),
      (fu_op === MDU_MULTU) -> u_mul(31, 0),
      (fu_op === MDU_DIV)   -> s_div,
      (fu_op === MDU_DIVU)  -> u_div))
  }

  io.fu_in.ready := Y
  io.working := fu_valid

  io.fu_out.valid := fu_valid
  io.fu_out.bits := fu_in.wb
  io.fu_out.bits.v  := wb_rd
  io.fu_out.bits.wen := wb_rd
  io.fu_out.bits.data := result

  when (!io.fu_in.fire() && io.fu_out.valid) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_MDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "MDU")
    printv(io.fu_in, "MDU.fu_in")
    printv(io.fu_out, "MDU.fu_out")
  }
}
