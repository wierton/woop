package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

class PRALUPipelineStage extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_FU_IO))
    val fu_out = DecoupledIO(new PRALU_FU_IO)
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)
  io.fu_in.ready := io.fu_out.ready || !fu_valid
  io.fu_out.valid := fu_valid && (fu_in.ops.fu_type === FU_BRU || fu_in.ops.fu_type === FU_MDU)
  io.fu_out.bits := fu_in
  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }
}

class PRALU extends Module {
  val io = IO(new Bundle {
    val rs_data = Flipped(ValidIO(Output(UInt(conf.xprlen.W))))
    val rt_data = Flipped(ValidIO(Output(UInt(conf.xprlen.W))))
    val fu_in = Flipped(DecoupledIO(new BRIDU_PRALU_IO))
    val fu_out = DecoupledIO(new PRALU_LSMDU_IO)
    val bp = ValidIO(new BypassIO)
    val ex_flush = ValidIO(new FlushIO)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val iaddr = Flipped(new TLBTransaction)
  })

  val rs_ready = Mux((io.fu_in.bits.op1_sel === OP1_RS) ||
    (io.fu_in.bits.op1_sel === OP1_RSO) ||
    (io.fu_in.bits.op2_sel === OP2_RS), io.rs_data.valid, Y)
  val rt_ready = Mux((io.fu_in.bits.op1_sel === OP1_RT) ||
    (io.fu_in.bits.op2_sel === OP2_RT), io.rt_data.valid, Y)
  val reg_ready = rs_ready && rt_ready

  /* prepare operands */
  val instr = io.fu_in.bits.wb.instr
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))

  val op1_data = Mux1H(Array(
    (io.fu_in.bits.op1_sel === OP1_RS)  -> io.rs_data.bits,
    (io.fu_in.bits.op1_sel === OP1_RT)  -> io.rt_data.bits,
    (io.fu_in.bits.op1_sel === OP1_RSO) -> (io.rs_data.bits + se_imm),
  )).asUInt

  val op2_data = Mux1H(Array(
    (io.fu_in.bits.op2_sel === OP2_RS)  -> io.rs_data.bits,
    (io.fu_in.bits.op2_sel === OP2_RT)  -> io.rt_data.bits,
    (io.fu_in.bits.op2_sel === OP2_IMI) -> se_imm,
    (io.fu_in.bits.op2_sel === OP2_IMU) -> ue_imm,
    (io.fu_in.bits.op2_sel === OP2_IMZ) -> ze_imm,
    (io.fu_in.bits.op2_sel === OP2_SA)  -> shamt_ext,
  )).asUInt

  /* PRU IO */
  val pru = Module(new PRU)
  pru.io.iaddr <> io.iaddr
  pru.io.fu_in.valid := io.fu_in.fire() && (
    io.fu_in.bits.fu_type === FU_PRU || io.fu_in.bits.fu_type === FU_LSU)
  pru.io.fu_in.bits.wb := io.fu_in.bits.wb
  pru.io.fu_in.bits.ops.fu_type := io.fu_in.bits.fu_type
  pru.io.fu_in.bits.ops.fu_op := io.fu_in.bits.fu_op
  pru.io.fu_in.bits.ops.op1 := op1_data
  pru.io.fu_in.bits.ops.op2 := op2_data
  pru.io.fu_in.bits.ex := io.fu_in.bits.ex
  pru.io.fu_out.ready := io.fu_out.ready
  pru.io.br_flush <> io.br_flush

  /* ALU IO */
  val alu = Module(new ALU)
  alu.io.ex_flush <> pru.io.ex_flush
  alu.io.fu_in.valid := io.fu_in.fire() && io.fu_in.bits.fu_type === FU_ALU
  alu.io.fu_in.bits.wb := io.fu_in.bits.wb
  alu.io.fu_in.bits.ops.fu_type := io.fu_in.bits.fu_type
  alu.io.fu_in.bits.ops.fu_op := io.fu_in.bits.fu_op
  alu.io.fu_in.bits.ops.op1 := op1_data
  alu.io.fu_in.bits.ops.op2 := op2_data
  alu.io.fu_in.bits.ex := io.fu_in.bits.ex
  alu.io.fu_out.ready := io.fu_out.ready

  /* PipelineStage */
  val psu = Module(new PRALUPipelineStage)
  psu.io.fu_in.valid := io.fu_in.fire()
  psu.io.fu_in.bits.wb := io.fu_in.bits.wb
  psu.io.fu_in.bits.ops.fu_type := io.fu_in.bits.fu_type
  psu.io.fu_in.bits.ops.fu_op := io.fu_in.bits.fu_op
  psu.io.fu_in.bits.ops.op1 := op1_data
  psu.io.fu_in.bits.ops.op2 := op2_data
  psu.io.fu_in.bits.ex := io.fu_in.bits.ex
  psu.io.fu_out.ready := io.fu_out.ready

  /* PRALU IO */
  io.fu_in.ready := reg_ready && pru.io.fu_in.ready &&
    alu.io.fu_in.ready && psu.io.fu_in.ready
  io.fu_out.valid := alu.io.fu_out.valid || pru.io.fu_out.valid || psu.io.fu_out.valid
  io.fu_out.bits.wb := Mux1H(Array(
    alu.io.fu_out.valid -> alu.io.fu_out.bits.wb,
    pru.io.fu_out.valid -> pru.io.fu_out.bits.wb,
    psu.io.fu_out.valid -> psu.io.fu_out.bits.wb))
  io.fu_out.bits.ops := psu.io.fu_out.bits.ops
  io.fu_out.bits.is_cached := pru.io.fu_out.bits.is_cached
  io.fu_out.bits.paddr := pru.io.fu_out.bits.paddr
  io.ex_flush <> pru.io.ex_flush

  /* bypass */
  io.bp.valid := alu.io.fu_out.valid || pru.io.fu_out.valid
  io.bp.bits.v := io.fu_out.bits.wb.v
  io.bp.bits.wen := io.fu_out.bits.wb.wen
  io.bp.bits.rd_idx := io.fu_out.bits.wb.rd_idx
  io.bp.bits.data :=  io.fu_out.bits.wb.data

  /* cp0 exception */
  pru.io.exinfo.valid := io.fu_out.valid
  pru.io.exinfo.bits := Mux1H(Array(
    alu.io.fu_out.valid -> alu.io.fu_out.bits.ex,
    pru.io.fu_out.valid -> pru.io.fu_out.bits.ex))

  if (conf.log_PRALU) {
    when (TraceTrigger()) { dump() }
  }

  def dump():Unit = {
    printf("%d: PRALU: io.{rs_data[%d]=%x, rt_data[%d]=%x}, shamt_ext=%x, se_imm=%x, ze_imm=%x, ue_imm=%x, op1_data=%x, op2_data=%x\n", GTimer(), io.rs_data.valid, io.rs_data.bits, io.rt_data.valid, io.rt_data.bits, shamt_ext, se_imm, ze_imm, ue_imm, op1_data, op2_data)
    instr.dump("PRALU.instr")
    psu.io.fu_in.dump("PRALU.psu.io.fu_in")
    psu.io.fu_out.dump("PRALU.psu.io.fu_out")
    io.fu_in.dump("PRALU.io.fu_in")
    io.fu_out.dump("PRALU.io.fu_out")
    io.bp.dump("PRALU.io.bp")
    io.ex_flush.dump("PRALU.io.ex_flush")
    io.iaddr.dump("PRALU.io.iaddr")
    alu.io.fu_in.dump("ALU.io.fu_in")
    alu.io.fu_out.dump("ALU.io.fu_out")
    pru.io.fu_in.dump("PRU.io.fu_in")
    pru.io.fu_out.dump("PRU.io.fu_out")
  }
  assert (AtMost1H(alu.io.fu_out.valid, pru.io.fu_out.valid, psu.io.fu_out.valid))
}
