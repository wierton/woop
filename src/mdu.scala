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

class MDUStageData extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(32.W))
  val wb = new WriteBackIO
}

class MDU extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new EHU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
    val divider = new DividerIO
    val multiplier = new MultiplierIO
    val can_log_now = Input(Bool())
  })

  val hi = RegInit(0.U(conf.xprlen.W))
  val lo = RegInit(0.U(conf.xprlen.W))

  val pipe_in = WireInit(0.U.asTypeOf(new MDUStageData))
  pipe_in.fu_op := io.fu_in.bits.ops.fu_op
  pipe_in.op1 := io.fu_in.bits.ops.op1
  pipe_in.wb := io.fu_in.bits.wb
  val fu_pipe = Pipe(io.fu_in.fire(), pipe_in, conf.mdu_stages)
  val fu_sz = RegInit(0.U(log2Ceil(conf.mdu_stages).W))
  fu_sz := fu_sz + io.fu_in.fire() - fu_pipe.valid

  val op1 = io.fu_in.bits.ops.op1
  val op2 = io.fu_in.bits.ops.op2
  val fu_in_op = io.fu_in.bits.ops.fu_op
  io.divider.data_dividend_valid := Y
  io.divider.data_divisor_valid := Y
  io.divider.data_dividend_bits := Mux(fu_in_op === MDU_DIV,
    op1.asTypeOf(SInt(40.W)).asUInt,
    op1.asTypeOf(UInt(40.W)))
  io.divider.data_divisor_bits := Mux(fu_in_op === MDU_DIV,
    op2.asTypeOf(SInt(40.W)).asUInt,
    op2.asTypeOf(UInt(40.W)))

  io.multiplier.data_a := Mux(fu_in_op === MDU_MULT,
    op1.asTypeOf(SInt(33.W)).asUInt,
    op1.asTypeOf(UInt(33.W)))
  io.multiplier.data_b := Mux(fu_in_op === MDU_MULT,
    op2.asTypeOf(SInt(33.W)).asUInt,
    op2.asTypeOf(UInt(33.W)))

  val fu_op = fu_pipe.bits.fu_op
  val wb_rd = fu_op.asTypeOf(new MDUOp).wb_reg === WB_RD
  val wb_hilo = !wb_rd
  val mul = io.multiplier.data_dout(63, 0)
  val div = io.divider.data_dout_bits(71, 40)
  val mod = io.divider.data_dout_bits(31, 0)
  val hilo = Cat(hi, lo)
  val wb_data = Mux1H(Array(
    (fu_op === MDU_MFHI) -> hi,
    (fu_op === MDU_MFLO) -> lo,
    (fu_op === MDU_MUL)  -> mul(31, 0)))

  when (fu_pipe.valid && wb_hilo) {
    hi := MuxLookup(fu_op, 0.U, Array(
      MDU_MTHI  -> fu_pipe.bits.op1,
      MDU_MTLO  -> hi,
      MDU_MULT  -> mul(63, 32),
      MDU_MULTU -> mul(63, 32),
      MDU_DIV   -> mod,
      MDU_DIVU  -> mod,
      MDU_MADD  -> (hilo + mul)(63, 32),
      MDU_MADDU -> (hilo + mul)(63, 32),
      MDU_MSUB  -> (hilo - mul)(63, 32),
      MDU_MSUBU -> (hilo - mul)(63, 32)))
    lo := MuxLookup(fu_op, 0.U, Array(
      MDU_MTHI  -> lo,
      MDU_MTLO  -> fu_pipe.bits.op1,
      MDU_MULT  -> mul(31, 0),
      MDU_MULTU -> mul(31, 0),
      MDU_DIV   -> div,
      MDU_DIVU  -> div,
      MDU_MADD  -> (hilo + mul)(31, 0),
      MDU_MADDU -> (hilo + mul)(31, 0),
      MDU_MSUB  -> (hilo - mul)(31, 0),
      MDU_MSUBU -> (hilo - mul)(31, 0)))
  }

  io.fu_in.ready := Y
  io.working := fu_sz =/= 0.U

  io.fu_out.valid := fu_pipe.valid
  io.fu_out.bits := fu_pipe.bits.wb
  io.fu_out.bits.v  := wb_rd
  io.fu_out.bits.wen := wb_rd
  io.fu_out.bits.data := wb_data

  if (conf.log_MDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "MDU")
    printv(io.fu_in, "MDU.fu_in")
    printv(io.fu_out, "MDU.fu_out")
  }
}
