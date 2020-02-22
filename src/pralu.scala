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
    val lsmdu = DecoupledIO(new PRALU_LSMDU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  /* register set */
  val sb = Mem(32, Bool())
  val rf = Mem(32, UInt(conf.xprlen.W))
  val pralu_rd_idx = RegInit(0.U(log2Ceil(32).W))

  def read_register(idx:UInt) = {
    val is_zero = idx === 0.U
    val reg_match = !sb(idx)
    val ready = is_zero || reg_match
    val data = Mux1H(Array(
      (is_zero) -> 0.U,
      (reg_match) -> rf(idx))

    Cat(data, ready)
  }

  /* read registers */
  val rs_data_status = Mux(
    (io.idu.bits.instr.rs_idx === 0.U),
    Mux((io.idu.bits.instr.rs_idx === pralu_rd_idx), Cat(0.U(32.W), Y),
    Cat(0.U(32.W), Y),
  ))
  val instr = io.idu.bits.instr.asTypeOf(new Instr)
  val op1_idx = Mux1H(Array(
    (io.idu.bits.op1_sel === OP1_RS) -> instr.rs_idx,
    (io.idu.bits.op1_sel === OP1_RT) -> instr.rt_idx,
    (io.idu.bits.op1_sel === OP1_IMU) -> 0.U
  )).asUInt

  val op2_idx = Mux1H(Array(
    (io.idu.bits.op2_sel === OP2_RS)  -> instr.rs_idx,
    (io.idu.bits.op2_sel === OP2_RT)  -> instr.rt_idx,
    )).asUInt

  val op1_reg_status = read_register(op1_idx)
  val op2_reg_status = read_register(op2_idx)

  /* prepare operands */
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))

  val op1_data_status = Mux1H(Array(
    (io.idu.bits.op1_sel === OP1_RS) -> op1_reg_status,
    (io.idu.bits.op1_sel === OP1_RT) -> op1_reg_status,
    (io.idu.bits.op1_sel === OP1_IMU) -> Cat(ue_imm, Y)
  )).asUInt

  val op2_data_status = Mux1H(Array(
    (io.idu.bits.op2_sel === OP2_RS)  -> op2_reg_status,
    (io.idu.bits.op2_sel === OP2_RT)  -> op2_reg_status,
    (io.idu.bits.op2_sel === OP2_IMI) -> Cat(se_imm, Y),
    (io.idu.bits.op2_sel === OP2_IMZ) -> Cat(ze_imm, Y),
    (io.idu.bits.op2_sel === OP2_SA)  -> Cat(shamt_ext, Y)
  )).asUInt

  val op1_data = op1_data_status(32, 1)
  val op2_data = op2_data_status(32, 1)
  val op_ready = io.idu.valid && op1_data_status(0) && op2_data_status(0)

  val alu = new ALU
  val pru = new PRU
}
