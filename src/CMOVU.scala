package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._


class CMOVU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val isu = Flipped(DecoupledIO(new ISU_CMOVU_IO));
    val wbu = DecoupledIO(new CMOVU_WBU_IO);
    val bypass = ValidIO(new BypassIO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val datain = RegEnable(next=io.isu.bits, enable=io.isu.fire());
  val isu_valid = RegNext(next=io.isu.fire(), init=false.B);
  val in_stage_1 = isu_valid;
  val fu_op = datain.fu_op;
  val op1 = datain.op1;
  val op2 = datain.op2;
  val reg_dest_idx = datain.reg_dest_idx;

  io.wbu.bits.need_wb := Mux1H(Array(
    (fu_op === CMOVU_MOVN) -> (op2 =/= 0.U),
    (fu_op === CMOVU_MOVZ) -> (op2 === 0.U)
  ));

  io.isu.ready := !in_stage_1 || io.wbu.ready;

  io.bypass.valid := io.wbu.valid;
  io.bypass.bits.wen := io.wbu.bits.need_wb;
  io.bypass.bits.reg_dest_idx := io.wbu.bits.reg_dest_idx;
  io.bypass.bits.data := io.wbu.bits.data;

  io.wbu.bits.npc := datain.npc;
  io.wbu.bits.data := op1;
  io.wbu.bits.reg_dest_idx := reg_dest_idx;
  io.wbu.valid := isu_valid && !io.flush.valid;

  when(io.isu.fire()) {
    log("[CMOVU] [CPC] >>>>>> %x <<<<<<\n", io.isu.bits.npc - 4.U);
  }
}

