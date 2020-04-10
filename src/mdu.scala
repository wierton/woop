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
  val id = Output(UInt(log2Ceil(conf.div_stages).W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(32.W))
  val wb = new WriteBackIO
}

class MDUStageIn extends Bundle {
  val id = Output(UInt(log2Ceil(conf.div_stages).W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(32.W))
  val op2 = Output(UInt(32.W))
  val wb = new WriteBackIO
}

class MDUStageOut extends Bundle {
  val id = Output(UInt(log2Ceil(conf.div_stages).W))
  val hi = Output(UInt(32.W))
  val lo = Output(UInt(32.W))
  val op1 = Output(UInt(32.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val wb = new WriteBackIO
}

class MDU_Multiplier extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new MDUStageIn))
    val fu_out = ValidIO(new MDUStageOut)
    val multiplier = new MultiplierIO
    val can_log_now = Input(Bool())
  })

  val op1 = io.fu_in.bits.op1
  val op2 = io.fu_in.bits.op2
  val pipe_in = WireInit(0.U.asTypeOf(new MDUStageData))
  pipe_in.fu_op := io.fu_in.bits.fu_op
  pipe_in.op1 := io.fu_in.bits.op1
  pipe_in.wb := io.fu_in.bits.wb
  val fu_pipe = Pipe(io.fu_in.fire(), pipe_in, conf.mul_stages)

  io.multiplier.data_a := Mux(pipe_in.fu_op === MDU_MULT,
    op1.asTypeOf(SInt(33.W)).asUInt,
    op1.asTypeOf(UInt(33.W)))
  io.multiplier.data_b := Mux(pipe_in.fu_op === MDU_MULT,
    op2.asTypeOf(SInt(33.W)).asUInt,
    op2.asTypeOf(UInt(33.W)))

  io.fu_in.ready := Y
  io.fu_out.valid := fu_pipe.valid
  io.fu_out.bits.fu_op := fu_pipe.bits.fu_op
  io.fu_out.bits.wb := fu_pipe.bits.wb
  io.fu_out.bits.id := fu_pipe.bits.id
  io.fu_out.bits.op1 := fu_pipe.bits.op1
  io.fu_out.bits.hi := io.multiplier.data_dout(63, 32)
  io.fu_out.bits.lo := io.multiplier.data_dout(31, 0)

  if (conf.log_MDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "MDU.D")
    printv(io.fu_in, "MDU.D.fu_in")
    printv(io.fu_out, "MDU.D.fu_out")
  }
}

class MDU_Divider extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new MDUStageIn))
    val fu_out = ValidIO(new MDUStageOut)
    val divider = new DividerIO
    val can_log_now = Input(Bool())
  })

  val queue = Module(new Queue(new MDUStageData, conf.div_stages))
  queue.io.enq.valid := io.divider.dividend_fire() && io.divider.divisor_fire()
  queue.io.enq.bits.id := io.fu_in.bits.id
  queue.io.enq.bits.op1 := io.fu_in.bits.op1
  queue.io.enq.bits.fu_op := io.fu_in.bits.fu_op
  queue.io.enq.bits.wb := io.fu_in.bits.wb
  queue.io.deq.ready := io.divider.data_dout_valid

  val op1 = io.fu_in.bits.op1
  val op2 = io.fu_in.bits.op2
  val fu_op = io.fu_in.bits.fu_op
  io.divider.data_dividend_valid := Y
  io.divider.data_divisor_valid := Y
  io.divider.data_dividend_bits := Mux(fu_op === MDU_DIV,
    op1.asTypeOf(SInt(40.W)).asUInt,
    op1.asTypeOf(UInt(40.W)))
  io.divider.data_divisor_bits := Mux(fu_op === MDU_DIV,
    op2.asTypeOf(SInt(40.W)).asUInt,
    op2.asTypeOf(UInt(40.W)))

  io.fu_in.ready := io.divider.data_dividend_ready && io.divider.data_divisor_ready
  io.fu_out.valid := io.divider.data_dout_valid
  io.fu_out.bits.wb := queue.io.deq.bits.wb
  io.fu_out.bits.fu_op := queue.io.deq.bits.fu_op
  io.fu_out.bits.id := queue.io.deq.bits.id
  io.fu_out.bits.op1 := queue.io.deq.bits.op1
  io.fu_out.bits.hi := io.divider.data_dout_bits(71, 40)
  io.fu_out.bits.lo := io.divider.data_dout_bits(31, 0)

  if (conf.log_MDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "MDU.M")
    printv(io.fu_in, "MDU.M.fu_in")
    printv(io.fu_out, "MDU.M.fu_out")
  }
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

  val is_div = io.fu_in.bits.ops.fu_op === MDU_DIV ||
    io.fu_in.bits.ops.fu_op === MDU_DIVU

  val fu_sz = RegInit(0.U(32.W))
  val mduid_width = log2Ceil(conf.div_stages)
  val hi = RegInit(0.U(conf.xprlen.W))
  val lo = RegInit(0.U(conf.xprlen.W))
  val mduid = RegInit(0.U(mduid_width.W))
  val multiplier = Module(new MDU_Multiplier)
  val divider = Module(new MDU_Divider)
  val rob = Module(new ROB(new MDUStageOut, mduid_width, 2))

  multiplier.io.can_log_now := io.can_log_now
  multiplier.io.fu_in.valid := io.fu_in.valid && !is_div
  multiplier.io.fu_in.bits.fu_op := io.fu_in.bits.ops.fu_op
  multiplier.io.fu_in.bits.id := mduid
  multiplier.io.fu_in.bits.op1 := io.fu_in.bits.ops.op1
  multiplier.io.fu_in.bits.op2 := io.fu_in.bits.ops.op2
  multiplier.io.fu_in.bits.wb := io.fu_in.bits.wb
  multiplier.io.multiplier <> io.multiplier

  divider.io.can_log_now := io.can_log_now
  divider.io.fu_in.valid := io.fu_in.valid && is_div
  divider.io.fu_in.bits.fu_op := io.fu_in.bits.ops.fu_op
  divider.io.fu_in.bits.id := mduid
  divider.io.fu_in.bits.op1 := io.fu_in.bits.ops.op1
  divider.io.fu_in.bits.op2 := io.fu_in.bits.ops.op2
  divider.io.fu_in.bits.wb := io.fu_in.bits.wb
  divider.io.divider <> io.divider

  rob.io.flush := N
  rob.io.deq.ready := Y

  rob.io.enq(0).valid := multiplier.io.fu_out.valid
  rob.io.enq(0).bits.id := multiplier.io.fu_out.bits.id
  rob.io.enq(0).bits.data := multiplier.io.fu_out.bits

  rob.io.enq(1).valid := divider.io.fu_out.valid
  rob.io.enq(1).bits.id := divider.io.fu_out.bits.id
  rob.io.enq(1).bits.data := divider.io.fu_out.bits

  io.fu_in.ready := !is_div || divider.io.fu_in.ready
  when (io.fu_in.fire()) { mduid := mduid + 1.U }
  fu_sz := fu_sz + io.fu_in.fire() - io.fu_out.valid

  val fu_op = rob.io.deq.bits.fu_op
  val wb_rd = fu_op.asTypeOf(new MDUOp).wb_reg === WB_RD
  val wb_hilo = !wb_rd
  val out_hi = rob.io.deq.bits.hi
  val out_lo = rob.io.deq.bits.lo
  val mul = Cat(out_hi, out_lo)
  val hilo = Cat(hi, lo)
  val wb_data = Mux1H(Array(
    (fu_op === MDU_MFHI) -> hi,
    (fu_op === MDU_MFLO) -> lo,
    (fu_op === MDU_MUL)  -> mul(31, 0)))

  when (rob.io.deq.valid && wb_hilo) {
    hi := MuxLookup(fu_op, 0.U, Array(
      MDU_MTHI  -> rob.io.deq.bits.op1,
      MDU_MTLO  -> hi,
      MDU_MULT  -> out_hi,
      MDU_MULTU -> out_hi,
      MDU_DIV   -> out_hi,
      MDU_DIVU  -> out_hi,
      MDU_MADD  -> (hilo + mul)(63, 32),
      MDU_MADDU -> (hilo + mul)(63, 32),
      MDU_MSUB  -> (hilo - mul)(63, 32),
      MDU_MSUBU -> (hilo - mul)(63, 32)))
    lo := MuxLookup(fu_op, 0.U, Array(
      MDU_MTHI  -> lo,
      MDU_MTLO  -> rob.io.deq.bits.op1,
      MDU_MULT  -> out_lo,
      MDU_MULTU -> out_lo,
      MDU_DIV   -> out_lo,
      MDU_DIVU  -> out_lo,
      MDU_MADD  -> (hilo + mul)(31, 0),
      MDU_MADDU -> (hilo + mul)(31, 0),
      MDU_MSUB  -> (hilo - mul)(31, 0),
      MDU_MSUBU -> (hilo - mul)(31, 0)))
  }

  io.working := fu_sz =/= 0.U

  io.fu_out.valid := rob.io.deq.valid
  io.fu_out.bits := rob.io.deq.bits.wb
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
