package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._


class ALU extends Module with UnitOpConstants {
  val io = IO(new Bundle {
    val bypass = ValidIO(new BypassIO)
    val fu_in = Flipped(DecoupledIO(new EXU_IO))
    val fu_out = DecoupledIO(new EXU_IO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)

  io.fu_in.ready := io.fu_out.ready || !fu_valid

  val fu_op = fu_in.fu_op
  val op1   = fu_in.op1
  val op2   = fu_in.op2
  val rd_idx = fu_in.rd_idx
  val op2_shamt = op2(REG_SZ - 1, 0).asUInt

  val result = Mux1H(Array(
    (fu_op === ALU_ADD)   -> (op1 + op2).asUInt,
    (fu_op === ALU_SUB)   -> (op1 - op2).asUInt,
    (fu_op === ALU_SLL)   -> (op1 << op2_shamt).asUInt,
    (fu_op === ALU_SRL)   -> (op1 >> op2_shamt).asUInt,
    (fu_op === ALU_SRA)   -> (op1.asSInt >> op2_shamt).asUInt,
    (fu_op === ALU_AND)   -> (op1 & op2).asUInt,
    (fu_op === ALU_OR)    -> (op1 | op2).asUInt,
    (fu_op === ALU_XOR)   -> (op1 ^ op2).asUInt,
    (fu_op === ALU_NOR)   -> ~(op1 ^ op2).asUInt,
    (fu_op === ALU_SLT)   -> (op1.asSInt < op2.asSInt).asUInt,
    (fu_op === ALU_SLTU)  -> (op1 < op2).asUInt,
    (fu_op === ALU_COPY1) -> op1.asUInt))


  io.bypass.valid := io.fu_out.valid
  io.bypass.bits.wen := Y
  io.bypass.bits.rd_idx := rd_idx
  io.bypass.bits.data := result

  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb.pc := fu_in.wb.pc
  io.fu_out.bits.wb.wen := Y
  io.fu_out.bits.wb.rd_idx := rd_idx
  io.fu_out.bits.wb.data := result
  io.fu_out.bits.fu_type := fu_in.fu_type
  io.fu_out.bits.fu_op := fu_in.fu_op
  io.fu_out.bits.op1 := fu_in.op1
  io.fu_out.bits.op2 := fu_in.op2
  io.fu_out.bits.ex := 0.U.asTypeOf(io.fu_out.bits.ex)

  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
}

