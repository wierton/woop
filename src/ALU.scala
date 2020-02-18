package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._


class ALU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val bypass = ValidIO(new BypassIO)
    val isu = Flipped(DecoupledIO(new ISU_ALU_IO))
    val wbu = DecoupledIO(new ALU_WBU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  val fu_in = RegEnable(next=io.isu.bits, enable=io.isu.fire())
  val fu_valid = RegInit(N)

  io.isu.ready := io.wbu.ready || !fu_valid

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


  io.bypass.valid := io.wbu.valid
  io.bypass.bits.wen := Y
  io.bypass.bits.rd_idx := rd_idx
  io.bypass.bits.data := result

  io.wbu.bits.npc := fu_in.npc
  io.wbu.bits.rd_idx := rd_idx
  io.wbu.bits.data := result
  io.wbu.valid := fu_valid

  when (io.flush.valid || (!io.isu.fire() && io.wbu.fire())) {
    fu_valid := N
  } .elsewhen(!io.flush.valid && io.isu.fire()) {
    fu_valid := Y
  }
}

