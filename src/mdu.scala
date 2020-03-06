package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._

class MDUOp extends Bundle
{
  val id     = UInt(4.W)
  val wb_reg = UInt(1.W)
}

class MDU extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
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
  io.fu_out.bits.pc := fu_in.wb.pc
  io.fu_out.bits.id := fu_in.wb.id
  io.fu_out.bits.v  := wb_rd
  io.fu_out.bits.wen := wb_rd
  io.fu_out.bits.rd_idx := fu_in.wb.rd_idx
  io.fu_out.bits.data := result
  io.fu_out.bits.instr := fu_in.wb.instr
  io.fu_out.bits.is_ds := N

  when (!io.fu_in.fire() && io.fu_out.valid) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_MDU) {
    when (TraceTrigger()) { dump() }
  }

  def dump():Unit = {
    printf("%d: MDU: fu_valid=%b, hi=%x, lo=%x, op1=%x, op2=%x, result=%x, wb_rd=%b, wb_hilo=%b, s_mul=%x, u_mul=%x, s_div=%x, s_mod=%x, u_div=%x, u_mod=%x\n", GTimer(), fu_valid, hi, lo, op1, op2, result, wb_rd, wb_hilo, s_mul, u_mul, s_div, s_mod, u_div, u_mod)
    io.fu_in.dump("MDU.io.fu_in")
    io.fu_out.dump("MDU.io.fu_out")
  }
}
