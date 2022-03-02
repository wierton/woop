package woop
package core

import chisel3._
import chisel3.util._
import chisel3.experimental._
import woop.consts._
import woop.configs._

import woop.utils._

class ISUModuleIO extends Bundle {
  val fu_in = Flipped(DecoupledIO(new IDU_ISU_IO))
  val fu_out = DecoupledIO(new ISU_EXU_IO)
  val br_flush = ValidIO(Output(new FlushIO))
  val rfio = new RegFileIO
  val can_log_now = Input(Bool())
}

class ISU extends Module {
  val io = IO(new ISUModuleIO)

  val bru = Module(new BRU)
  bru.io.can_log_now := io.can_log_now

  val instr = io.fu_in.bits.instr
  val instr_id = RegInit(0.U(conf.INSTR_ID_SZ.W))
  when (io.fu_out.fire()) { instr_id := instr_id + 1.U }

  /* rf io */
  io.rfio.rs_idx := instr.rs_idx
  io.rfio.rt_idx := instr.rt_idx
  io.rfio.wen := io.fu_out.fire() && io.fu_in.bits.opd_sel =/= OPD_X
  io.rfio.wid := instr_id
  io.rfio.rd_idx := MuxLookup(io.fu_in.bits.opd_sel, 0.U, Array(
    OPD_RD -> instr.rd_idx,
    OPD_RT -> instr.rt_idx,
    OPD_31 -> 31.U))

  /* fu_out IO */
  val rs_ready = Mux((io.fu_in.bits.op1_sel === OP1_RS) ||
    (io.fu_in.bits.op1_sel === OP1_RSO) ||
    (io.fu_in.bits.op2_sel === OP2_RS),
    io.rfio.rs_data.valid, Y)
  val rt_ready = Mux((io.fu_in.bits.op1_sel === OP1_RT) ||
    (io.fu_in.bits.op2_sel === OP2_RT),
    io.rfio.rt_data.valid, Y)
  val reg_ready = rs_ready && rt_ready

  /* prepare operands */
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))
  val rs_data = io.rfio.rs_data.bits
  val rt_data = io.rfio.rt_data.bits

  val op1_data = Mux1H(Array(
    (io.fu_in.bits.op1_sel === OP1_RS)  -> rs_data,
    (io.fu_in.bits.op1_sel === OP1_RT)  -> rt_data,
    (io.fu_in.bits.op1_sel === OP1_RSO) -> (rs_data + se_imm),
  )).asUInt

  val op2_data = Mux1H(Array(
    (io.fu_in.bits.op2_sel === OP2_RS)  -> rs_data,
    (io.fu_in.bits.op2_sel === OP2_RT)  -> rt_data,
    (io.fu_in.bits.op2_sel === OP2_IMI) -> se_imm,
    (io.fu_in.bits.op2_sel === OP2_IMU) -> ue_imm,
    (io.fu_in.bits.op2_sel === OP2_IMZ) -> ze_imm,
    (io.fu_in.bits.op2_sel === OP2_SA)  -> shamt_ext,
    (io.fu_in.bits.op2_sel === OP2_COP) -> instr.rt_idx,
  )).asUInt

  val is_delayslot = RegInit(N)
  val br_target = RegInit(0.U(32.W))
  val exu_wb = WireInit(0.U.asTypeOf(new WriteBackIO))
  when (io.fu_out.fire() && io.fu_in.bits.fu_type === FU_BRU) {
    is_delayslot := Y
    br_target := Mux(io.br_flush.valid,
      io.br_flush.bits.br_target,
      io.fu_out.bits.wb.pc + 8.U)
  } .elsewhen (io.fu_out.fire()) {  is_delayslot := N  }
  exu_wb := 0.U.asTypeOf(new WriteBackIO)
  exu_wb.pc := io.fu_in.bits.pc
  exu_wb.instr := io.fu_in.bits.instr
  exu_wb.is_ds := is_delayslot

  io.fu_in.ready := io.fu_out.ready && reg_ready
  io.fu_out.valid := io.fu_in.valid && reg_ready
  io.fu_out.bits.ops.fu_type := io.fu_in.bits.fu_type
  io.fu_out.bits.ops.fu_op := io.fu_in.bits.fu_op
  io.fu_out.bits.ops.op1 := op1_data
  io.fu_out.bits.ops.op2 := op2_data
  io.fu_out.bits.wb := Mux(io.fu_in.bits.fu_type === FU_BRU,
    bru.io.fu_out.bits.wb, exu_wb)
  io.fu_out.bits.wb.npc := Mux(is_delayslot, br_target,
    io.fu_out.bits.wb.pc + 4.U)
  io.fu_out.bits.wb.id := instr_id
  io.fu_out.bits.wb.rd_idx := io.rfio.rd_idx
  io.fu_out.bits.ex := io.fu_in.bits.ex

  /* branch */
  bru.io.fu_in.valid := io.fu_in.valid && io.fu_in.bits.fu_type === FU_BRU
  bru.io.fu_in.bits.wb := exu_wb
  bru.io.fu_in.bits.ops := io.fu_out.bits.ops

  io.br_flush.valid := bru.io.fu_out.valid && io.fu_out.fire()
  io.br_flush.bits.br_target := bru.io.fu_out.bits.br_target

  if (conf.log_ISU) {
    when (io.can_log_now) { dump() }
  }

  def dump() = {
    printv(this, "ISU")
  }
}
