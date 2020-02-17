package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._


class ALU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val bypass = ValidIO(new BypassIO);
    val isu = Flipped(DecoupledIO(new ISU_ALU_IO));
    val wbu = DecoupledIO(new ALU_WBU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val isu_valid = RegNext(next=io.isu.fire(), init=false.B);
  val in_stage_1 = isu_valid;
  val datain = RegEnable(next=io.isu.bits, enable=io.isu.fire());

  val fu_op = datain.fu_op;
  val op1   = datain.op1;
  val op2   = datain.op2;
  val reg_dest_idx = datain.reg_dest_idx;
  val op2_shamt = op2(REG_SZ - 1, 0).asUInt;

  when(io.isu.fire()) {
    log("[ALU] [CPC] >>>>>> %x <<<<<<\n", io.isu.bits.npc - 4.U);
    log("[ALU] fu_op:%x\n", io.isu.bits.fu_op);
    log("[ALU] op1:%x\n", io.isu.bits.op1);
    log("[ALU] op2:%x\n", io.isu.bits.op2);
    log("[ALU] reg_dest_idx:%x\n", io.isu.bits.reg_dest_idx);
  }

  when(io.wbu.valid) {
    log("[ALU] wb valid, addr:%x, data:%x\n", io.wbu.bits.reg_dest_idx, io.wbu.bits.data);
  }

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
    (fu_op === ALU_COPY1) -> op1.asUInt,
    ));

  io.isu.ready := !in_stage_1 || io.wbu.ready;

  io.bypass.valid := io.wbu.valid;
  io.bypass.bits.wen := true.B;
  io.bypass.bits.reg_dest_idx := io.wbu.bits.reg_dest_idx;
  io.bypass.bits.data := io.wbu.bits.data;

  io.wbu.bits.npc := datain.npc;
  io.wbu.bits.reg_dest_idx := reg_dest_idx;
  io.wbu.bits.data := result;
  io.wbu.valid := in_stage_1 && !io.flush.valid;
}

