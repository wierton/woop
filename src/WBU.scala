package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class WBU extends Module {
  val io = IO(new Bundle {
    val alu = Flipped(DecoupledIO(new ALU_WBU_IO));
    val mdu = Flipped(DecoupledIO(new MDU_WBU_IO));
    val lsu = Flipped(DecoupledIO(new LSU_WBU_IO));
    val bru = Flipped(DecoupledIO(new BRU_WBU_IO));
    val wb = ValidIO(new WriteBackIO);
    val flush = ValidIO(new FlushIO);
  });

  val wb_npc = RegInit(0.U(conf.xprlen.W));
  val wb_addr = RegInit(0.U(conf.xprlen.W));
  val wb_data = RegInit(0.U(conf.xprlen.W));
  val wb_wen = RegInit(false.B);

  // valid signals
  val alu_valid = RegNext(next=io.alu.fire(), init=false.B);
  val mdu_valid = RegNext(next=io.mdu.fire(), init=false.B);
  val lsu_valid = RegNext(next=io.lsu.fire(), init=false.B);
  val bru_valid = RegNext(next=io.bru.fire(), init=false.B);

  // flush_valid
  val flush_valid = RegEnable(next=io.bru.bits.need_br, init=false.B, enable=io.bru.fire());

  // fu_valid
  val fu_valid = alu_valid || mdu_valid || lsu_valid || bru_valid

  assert(AtMost1H(io.alu.valid, io.mdu.valid, io.lsu.valid, io.bru.valid));

  io.alu.ready := true.B;
  io.mdu.ready := true.B;
  io.lsu.ready := true.B;
  io.bru.ready := true.B;

  // wb
  io.wb.bits.npc := wb_npc;
  io.wb.bits.reg_dest_idx := wb_addr;
  io.wb.bits.data := wb_data;
  io.wb.bits.wen := wb_wen;
  io.wb.valid := fu_valid;

  // branch
  io.flush.valid := flush_valid;
  io.flush.bits.br_target := RegEnable(next=io.bru.bits.br_target, init=0.U, enable=io.bru.fire());

  when(flush_valid) { flush_valid := false.B; }

  // writeback signals
  when(io.alu.fire()) {
    log("[WBU] [CPC] >>>>>> %x <<<<<<\n", io.alu.bits.npc - 4.U);
    log("[WBU] <- [ALU]: addr:%x, data:%x\n", io.alu.bits.reg_dest_idx, io.alu.bits.data);
    wb_npc := io.alu.bits.npc;
    wb_addr := io.alu.bits.reg_dest_idx;
    wb_data := io.alu.bits.data;
    wb_wen := true.B;
  }

  when(io.mdu.fire()) {
    log("[WBU] [CPC] >>>>>> %x <<<<<<\n", io.mdu.bits.npc - 4.U);
    log("[WBU] <- [MDU]: addr:%x, data:%x\n", io.mdu.bits.reg_dest_idx, io.mdu.bits.data);
    wb_npc := io.mdu.bits.npc;
    wb_addr := io.mdu.bits.reg_dest_idx;
    wb_data := io.mdu.bits.data;
    wb_wen := io.mdu.bits.need_wb;
  }

  when(io.lsu.fire()) {
    log("[WBU] [CPC] >>>>>> %x <<<<<<\n", io.lsu.bits.npc - 4.U);
    log("[WBU] <- [LSU]: addr:%x, data:%x\n", io.lsu.bits.reg_dest_idx, io.lsu.bits.data);
    wb_npc := io.lsu.bits.npc;
    wb_addr := io.lsu.bits.reg_dest_idx;
    wb_data := io.lsu.bits.data;
    wb_wen := io.lsu.bits.need_wb;
  }

  when(io.bru.fire()) {
    log("[WBU] [CPC] >>>>>> %x <<<<<<\n", io.bru.bits.npc - 4.U);
    log("[WBU] <- [BRU]: addr:%x, data:%x\n", io.bru.bits.reg_dest_idx, io.bru.bits.data);
    wb_npc := io.bru.bits.npc;
    wb_addr := io.bru.bits.reg_dest_idx;
    wb_data := io.bru.bits.data;
    wb_wen := io.bru.bits.need_wb;
  }

  when(io.wb.valid) {
    log("[WBU] wb valid, addr=%x, data=%x\n", wb_addr, wb_data);
  }

  when(io.bru.fire()) {
    log("[WBU] need_br:%x, br_target:%x\n", io.bru.bits.need_br, io.bru.bits.br_target);
  }

  when(io.flush.valid) {
    log("[WBU] flush_valid: io.flush.br_target:%x\n", io.flush.bits.br_target);
  }
}

