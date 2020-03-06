package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._


class ALU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_FU_IO))
    val fu_out = DecoupledIO(new PRALU_OUT)
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)

  io.fu_in.ready := io.fu_out.ready || !fu_valid

  val fu_op = fu_in.ops.fu_op
  val op1   = fu_in.ops.op1
  val op2   = fu_in.ops.op2
  val rd_idx = fu_in.wb.rd_idx
  val op2_sa = op2(REG_SZ - 1, 0).asUInt

  val result = Mux1H(Array(
    (fu_op === ALU_ADD)  -> (op1 + op2).asUInt,
    (fu_op === ALU_SUB)  -> (op1 - op2).asUInt,
    (fu_op === ALU_SLL)  -> (op1 << op2_sa).asUInt,
    (fu_op === ALU_SRL)  -> (op1 >> op2_sa).asUInt,
    (fu_op === ALU_SRA)  -> (op1.asSInt >> op2_sa).asUInt,
    (fu_op === ALU_AND)  -> (op1 & op2).asUInt,
    (fu_op === ALU_OR)   -> (op1 | op2).asUInt,
    (fu_op === ALU_XOR)  -> (op1 ^ op2).asUInt,
    (fu_op === ALU_NOR)  -> ~(op1 | op2).asUInt,
    (fu_op === ALU_SLT)  -> (op1.asSInt < op2.asSInt).asUInt,
    (fu_op === ALU_SLTU) -> (op1 < op2).asUInt,
    (fu_op === ALU_LUI)  -> op2.asUInt,
    (fu_op === ALU_MOVN) -> op1.asUInt,
    (fu_op === ALU_MOVZ) -> op1.asUInt,
  ))

  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb.pc := fu_in.wb.pc
  io.fu_out.bits.wb.id := fu_in.wb.id
  io.fu_out.bits.wb.v  := Y
  io.fu_out.bits.wb.wen := MuxCase(Y, Array(
    (fu_op === ALU_MOVN) -> (op2 =/= 0.U),
    (fu_op === ALU_MOVZ) -> (op2 === 0.U)))
  io.fu_out.bits.wb.rd_idx := rd_idx
  io.fu_out.bits.wb.data := result
  io.fu_out.bits.wb.instr := fu_in.wb.instr
  io.fu_out.bits.wb.is_ds := fu_in.wb.is_ds
  io.fu_out.bits.ex := 0.U.asTypeOf(io.fu_out.bits.ex)

  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
}

