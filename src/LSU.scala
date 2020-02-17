package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class LSU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val mem = new MemIO;
    val isu = Flipped(DecoupledIO(new ISU_LSU_IO));
    val wbu = DecoupledIO(new LSU_WBU_IO);
    val bypass = ValidIO(new BypassIO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val isu_valid = RegNext(next=io.isu.fire(), init=false.B);
  val in_stage_1 = isu_valid;
  val datain = RegEnable(io.isu.bits, io.isu.fire());
  val fu_op = datain.fu_op;
  val data = datain.data;
  val addr = datain.base + io.isu.bits.offset;
  val reg_dest_idx = datain.reg_dest_idx;

  val lsuop = Wire(new LSUOp);
  lsuop := fu_op;

  val mem_resp_data = io.mem.resp.bits.data;

  val addr_ul  = Cat(addr(31, 2), 0.U(2.W));
  val addr_l2 = addr(1, 0);
  val is_ul = !lsuop.isAlign() && lsuop.isLeft();
  val u_dt = Mux(lsuop.isLeft(), addr_l2, ~addr_l2);

  val req_data = Mux(is_ul, data >> (~addr_l2 << 3), data);

  io.mem.req.valid := isu_valid && !io.flush.valid;
  io.mem.req.bits.func  := lsuop.func;
  io.mem.req.bits.addr  := Mux(is_ul, addr_ul, addr);
  io.mem.req.bits.wstrb := 0.U
  io.mem.req.bits.data  := req_data;

  import ExtOperation._
  val dat_b_se = mem_resp_data(7, 0).SExt(32);
  val dat_b_ze = mem_resp_data(7, 0).ZExt(32);
  val dat_h_se = mem_resp_data(15, 0).SExt(32);
  val dat_h_ze = mem_resp_data(15, 0).ZExt(32);

  val align_data = Mux1H(Array(
    (lsuop.getDtExt === LSU_B_SE) -> dat_b_se,
    (lsuop.getDtExt === LSU_B_ZE) -> dat_b_ze,
    (lsuop.getDtExt === LSU_H_SE) -> dat_h_se,
    (lsuop.getDtExt === LSU_H_ZE) -> dat_h_ze,
    (lsuop.getDtExt === LSU_W_SE) -> mem_resp_data,
    (lsuop.getDtExt === LSU_W_ZE) -> mem_resp_data,
    ));

  val ul_mask = Mux(addr_l2(1),
    Mux(addr_l2(0), "b1111".U(4.W), "b1110".U(4.W)),
    Mux(addr_l2(0), "b1100".U(4.W), "b1000".U(4.W)));

  val ur_mask = Mux(addr_l2(1),
    Mux(addr_l2(0), "b0001".U(4.W), "b0011".U(4.W)),
    Mux(addr_l2(0), "b0111".U(4.W), "b1111".U(4.W)));

  val mask = Mux(lsuop.isLeft(), ul_mask, ur_mask);

  val ul_data = mem_resp_data << ((~addr_l2) << 3);
  val u_data = Mux(lsuop.isLeft(), ul_data, mem_resp_data);
  val unalign_data = Cat(for(i <- 3 to 0 by -1) yield Mux(mask(i), u_data(i * 8 + 7, i * 8), data(i * 8 + 7, i * 8)));

  val wb_data = Mux(lsuop.isAlign(), align_data, unalign_data);

  when(io.isu.fire()) {
    log("[LSU] [CPC] >>>>>> %x <<<<<<\n", io.isu.bits.npc - 4.U);
  }
  when(isu_valid) {
    log("[LSU] fu_op:%x, func:%x, dt:%x, ext:%x, getDtExt:%x\n", fu_op, lsuop.func, lsuop.dt, lsuop.ext, lsuop.getDtExt);
    log("[LSU] dat_b_se:%x, dat_b_ze:%x, dat_h_se:%x, dat_h_ze:%x\n", dat_b_se, dat_b_ze, dat_h_se, dat_h_ze);
  }

  when(io.mem.req.fire()) {
    log("[LSU] ul_mask:%x, ur_mask:%x, ul_data:%x, u_data:%x\n", ul_mask, ur_mask, ul_data(31, 0), u_data);
    log("[LSU] align_data:%x, unalign_data:%x, wb_data:%x\n", align_data, unalign_data, wb_data);
  }
  when(io.mem.resp.valid) {
    log("[LSU] mem_resp_data:%x\n", mem_resp_data);
  }
  when(io.wbu.fire()) {
    log("[LSU] need_wb:%x, wb_data:%x, wb_idx:%x\n", lsuop.func === MX_RD, wb_data, reg_dest_idx);
  }

  io.mem.resp.ready := true.B;

  io.isu.ready := !in_stage_1 || io.wbu.ready;

  // bypass signals
  io.bypass.valid := io.wbu.valid;
  io.bypass.bits.wen := io.wbu.bits.need_wb;
  io.bypass.bits.reg_dest_idx := io.wbu.bits.reg_dest_idx;
  io.bypass.bits.data := io.wbu.bits.data;

  // wbu signals
  io.wbu.bits.npc := datain.npc;
  io.wbu.bits.need_wb := lsuop.func === MX_RD;
  io.wbu.bits.data := wb_data;
  io.wbu.bits.reg_dest_idx := reg_dest_idx;
  io.wbu.valid := (
    (lsuop.func === MX_WR && io.mem.req.fire()) ||
    (lsuop.func === MX_RD && io.mem.resp.valid)
  ) && !io.flush.valid;
}

