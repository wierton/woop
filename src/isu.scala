package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._


class ISU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new IDU_ISU_IO))
    val fu_out = DecoupledIO(new ISU_EXU_IO)
    val bru = Flipped(new ISU_BRU_IO)
    val rfio = new RegFileIO
  })

  val instr = io.fu_in.bits.instr
  val instr_id = RegInit(0.U(INSTR_ID_SZ.W))
  when (io.fu_out.fire()) { instr_id := instr_id + 1.U }

  /* rf io */
  io.rfio.rs_idx := instr.rs_idx
  io.rfio.rt_idx := instr.rt_idx
  io.rfio.wen := io.fu_out.fire() && io.fu_in.bits.opd_sel =/= OPD_X
  io.rfio.wid := instr_id
  io.rfio.rd_idx := MuxLookup(0.U, io.fu_in.bits.opd_sel,
    Array(OPD_RD -> instr.rd_idx,
          OPD_RT -> instr.rt_idx,
          OPD_31 -> 31.U))

  /* fu_out IO */
  val rs_ready = Mux((io.fu_in.bits.op1_sel === OP1_RS) ||
    (io.fu_in.bits.op1_sel === OP1_RSO) ||
    (io.fu_in.bits.op2_sel === OP2_RS), io.rs_data.valid, Y)
  val rt_ready = Mux((io.fu_in.bits.op1_sel === OP1_RT) ||
    (io.fu_in.bits.op2_sel === OP2_RT), io.rt_data.valid, Y)
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
  )).asUInt

  val is_delayslot = RegInit(N)
  val exu_wb = WireInit(0.U.asTypeOf(new WriteBackIO))
  when (io.br_flush.valid) { is_delayslot := Y }
  .elsewhen (io.fu_out.fire()) {  is_delayslot := N  }
  exu_wb := 0.U.asTypeOf(new WriteBackIO)
  exu_wb.pc := io.fu_in.bits.pc
  exu_wb.instr := io.fu_in.bits.instr
  exu_wb.fu_type := io.fu_in.bits.fu_type
  exu_wb.fu_op := io.fu_in.bits.fu_op
  exu_wb.is_ds := is_delayslot

  io.fu_in.ready := (io.fu_out.ready || !fu_valid) && reg_ready
  io.fu_out.valid := fu_valid && reg_ready
  io.fu_out.bits.ops.fu_type := io.fu_in.bits.fu_type
  io.fu_out.bits.ops.fu_op := io.fu_in.bits.fu_op
  io.fu_out.bits.ops.op1 := op1_data
  io.fu_out.bits.ops.op2 := op2_data
  io.fu_out.bits.wb := Mux(io.fu_in.bits.fu_type === FU_BRU,
    io.bru.fu_out.bits.wb, exu_wb)
  io.fu_out.bits.rd_idx := io.rfio.rd_idx
  io.fu_out.bits.ex := io.fu_in.bits.ex

  /* branch */
  io.bru.fu_in.valid := io.fu_in.bits.fu_op === FU_BRU
  io.bru.fu_in.wb := io.fu_out.bits.wb
  io.bru.fu_in.ops := io.fu_out.bits.ops

  io.br_flush.valid := io.bru.fu_out.valid && io.fu_out.fire()
  io.br_flush.br_target := io.bru.fu_out.bits.br_target
}
