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
    val fu_in = Flipped(DecoupledIO(new BRIDU_PRALU_IO))
    val fu_out = DecoupledIO(new PRALU_LSMDU_IO)
    val bp = new BypassIO
    val exinfo = ValidIO(new CP0Exception)
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val iaddr = Flipped(new TLBTransaction)
  })

  /* prepare operands */
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))

  val op1_data = Mux1H(Array(
    (io.bridu.bits.op1_sel === OP1_RS) -> io.rs_data.bits,
    (io.bridu.bits.op1_sel === OP1_RT) -> io.rt_data.bits,
    (io.bridu.bits.op1_sel === OP1_IMU) -> ue_imm,
  )).asUInt

  val op2_data = Mux1H(Array(
    (io.bridu.bits.op2_sel === OP2_RS)  -> io.rs_data.bits,
    (io.bridu.bits.op2_sel === OP2_RT)  -> io.rt_data.bits,
    (io.bridu.bits.op2_sel === OP2_IMI) -> se_imm,
    (io.bridu.bits.op2_sel === OP2_IMZ) -> ze_imm,
    (io.bridu.bits.op2_sel === OP2_SA)  -> shamt_ext,
  )).asUInt

  /* ALU IO */
  val alu = Module(new ALU)
  alu.io.fu_in.valid := io.bridu.valid && io.bridu.fu_type === FU_ALU
  alu.io.fu_in.bits.wb <> io.wb
  alu.io.fu_in.bits.ops.fu_type := io.fu_in.ops.fu_type
  alu.io.fu_in.bits.ops.fu_op := io.fu_in.ops.fu_op
  alu.io.fu_in.bits.ops.op1 := op1_data
  alu.io.fu_in.bits.ops.op2 := op2_data
  alu.io.fu_in.bits.ex := io.fu_in.bits.ex
  alu.io.fu_out.ready := io.fu_out.ready

  /* PRU IO */
  val pru = Module(new PRU)
  pru.io.iaddr <> io.iaddr
  pru.io.fu_in.valid := io.bridu.valid && (
    io.bridu.ops.fu_type === FU_PRU || io.bridu.ops.fu_type === FU_LSU)
  pru.io.fu_in.bits.wb <> io.wb
  pru.io.fu_in.bits.ops.fu_type := io.fu_in.ops.fu_type
  pru.io.fu_in.bits.ops.fu_op := io.fu_in.ops.fu_op
  pru.io.fu_in.bits.ops.op1 := op1_data
  pru.io.fu_in.bits.ops.op2 := op2_data
  pru.io.fu_in.bits.ex := io.fu_in.bits.ex
  pru.io.fu_out.ready := io.fu_out.ready

  /* pipeline stage for bru data */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire() &&
    (io.fu_in.ops.fu_type === FU_LSU || io.fu_in.ops.fu_type === FU_MDU)) {
    fu_valid := Y
  }

  /* PRALU IO */
  val rs_ready = MuxCase(Y, Array(
    (io.bridu.bits.op1_sel === OP1_RS) -> io.rs_data.valid,
    (io.bridu.bits.op1_sel === OP1_RT) -> io.rt_data.valid))
  val rt_ready = MuxCase(Y, Array(
    (io.bridu.bits.op2_sel === OP2_RS) -> io.rs_data.valid,
    (io.bridu.bits.op2_sel === OP2_RT) -> io.rt_data.valid))
  io.fu_in.ready := rs_ready && rt_ready && io.fu_out.ready
  io.fu_out.valid := alu.io.fu_out.valid || pru.io.fu_out.valid || fu_valid
  io.fu_out.wb := Mux1H(Array(
    alu.io.fu_out.valid -> alu.io.fu_out.wb,
    pru.io.fu_out.valid -> pru.io.fu_out.wb,
    fu_valid -> fu_in.wb))
  io.fu_out.ops := fu_in.ops

  /* cp0 exception */
  io.exinfo.valid := io.fu_out.valid
  io.exinfo.bits.ex := Mux1H(Array(
    alu.io.fu_out.valid -> alu.io.fu_out.ex,
    pru.io.fu_out.valid -> pru.io.fu_out.ex,
    fu_valid -> fu_in.ex))
  assert (AtMost1H(alu.io.fu_out.valid, pru.io.fu_out.valid, fu_valid))
}
