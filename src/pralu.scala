package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class PRALU extends Module {
  val io = IO(new Bundle {
    val bridu = Flipped(DecoupledIO(new BRIDU_PRALU_IO))
    val rs_data = Flipped(ValidIO(Output(conf.xprlen.W)))
    val rt_data = Flipped(ValidIO(Output(conf.xprlen.W)))
    val fu_in = Flipped(DecoupledIO(new EXU_IO))
    val fu_out = DecoupledIO(new EXU_IO)
    val bp = new BypassIO
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  /* prepare operands */
  val rs_data_status = Cat(io.rs_data.bits, io.rs_data.valid)
  val rt_data_status = Cat(io.rt_data.bits, io.rt_data.valid)
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))

  val op1_data_status = Mux1H(Array(
    (io.bridu.bits.op1_sel === OP1_RS) -> rs_data_status,
    (io.bridu.bits.op1_sel === OP1_RT) -> rt_data_status,
    (io.bridu.bits.op1_sel === OP1_IMU) -> Cat(ue_imm, Y)
  )).asUInt

  val op2_data_status = Mux1H(Array(
    (io.bridu.bits.op2_sel === OP2_RS)  -> rs_data_status,
    (io.bridu.bits.op2_sel === OP2_RT)  -> rt_data_status,
    (io.bridu.bits.op2_sel === OP2_IMI) -> Cat(se_imm, Y),
    (io.bridu.bits.op2_sel === OP2_IMZ) -> Cat(ze_imm, Y),
    (io.bridu.bits.op2_sel === OP2_SA)  -> Cat(shamt_ext, Y)
  )).asUInt

  val op1_data = op1_data_status(32, 1)
  val op2_data = op2_data_status(32, 1)
  val op_ready = io.bridu.valid && op1_data_status(0) && op2_data_status(0)

  val alu = Module(new ALU)
  alu.io.fu_in.valid := io.bridu.valid && io.bridu.fu_type === FU_ALU
  alu.io.fu_in.bits.wb <> io.wb
  alu.io.fu_in.bits.fu_type := io.fu_type
  alu.io.fu_in.bits.fu_op := io.fu_op
  alu.io.fu_in.bits.op1.valid := op1_data_status(0)
  alu.io.fu_in.bits.op1.data.bits := op1_data_status(32, 1)
  alu.io.fu_in.bits.op2.valid := op2_data_status(0)
  alu.io.fu_in.bits.op2.data.bits := op2_data_status(32, 1)

  val pru = Module(new PRU)
  pru.io.fu_in.valid := io.bridu.valid && io.bridu.fu_type === FU_PRU
  pru.io.fu_in.bits.wb <> io.wb
  pru.io.fu_in.bits.fu_type := io.fu_type
  pru.io.fu_in.bits.fu_op := io.fu_op
  pru.io.fu_in.bits.op1.valid := op1_data_status(0)
  pru.io.fu_in.bits.op1.data.bits := op1_data_status(32, 1)
  pru.io.fu_in.bits.op2.valid := op2_data_status(0)
  pru.io.fu_in.bits.op2.data.bits := op2_data_status(32, 1)

  /* pipeline stage for bru data */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }

  io.fu_out.valid := alu.io.fu_out.valid || pru.io.fu_out.valid || fu_valid
  io.fu.out.bits := Mux1H(Array(
    alu.io.fu_out.valid -> alu.io.fu_out.bits,
    pru.io.fu_out.valid -> pru.io.fu_out.bits,
    fu_valid -> fu_in))
  assert (AtMost1H(alu.io.fu_out.valid, pru.io.fu_out.valid, fu_valid))
}
