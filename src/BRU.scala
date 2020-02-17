package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._


class BRU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val isu = Flipped(DecoupledIO(new ISU_BRU_IO));
    val wbu = DecoupledIO(new BRU_WBU_IO);
    val bypass = ValidIO(new BypassIO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val datain = RegEnable(next=io.isu.bits, io.isu.fire());
  val isu_valid = RegNext(next=io.isu.fire(), init=false.B);
  val in_stage_1 = isu_valid;
  val fu_op = datain.fu_op;
  val npc = datain.npc;
  val rs_data = datain.rs_data;
  val rt_data = datain.rt_data;
  val addr = datain.addr;
  val se_off = datain.se_off;
  val reg_dest_idx = datain.reg_dest_idx;

  val I = (npc + (se_off << 2))(31, 0);
  val J = Cat(Seq(npc(OP_MSB, OP_LSB + 2), addr, 0.U(2.W)));
  val JR = rs_data;

  require(I.getWidth == conf.xprlen);
  require(J.getWidth == conf.xprlen);
  require(JR.getWidth == conf.xprlen);

  when(io.isu.fire()) {
    log("[BRU] [CPC] >>>>>> %x <<<<<<\n", io.isu.bits.npc - 4.U);
    log("[BRU] addr:%x\n", io.isu.bits.addr);
    log("[BRU] fu_op:%x\n", io.isu.bits.fu_op);
    log("[BRU] npc:%x\n", io.isu.bits.npc);
    log("[BRU] rs_data:%x\n", io.isu.bits.rs_data);
    log("[BRU] rt_data:%x\n", io.isu.bits.rt_data);
    log("[BRU] addr:%x\n", io.isu.bits.addr);
    log("[BRU] se_off:%x\n", io.isu.bits.se_off);
    log("[BRU] reg_dest_idx:%x\n", io.isu.bits.reg_dest_idx);
  }

  when(io.wbu.fire()) {
    log("[BRU] I addr:%x\n", I);
    log("[BRU] J addr:%x\n", J);
    log("[BRU] JR addr:%x\n", JR);
  }

  // Mux Result
  class MR {
    val need_br = Wire(Bool());
    val need_wb = Wire(Bool());
    val br_target = Wire(UInt(conf.xprlen.W));

    def this(in:UInt) {
      this();
      Tie(need_br, need_wb, br_target) := in;
    }
  }

  val mux_result_u = Mux1H(Array(
    (fu_op === BR_EQ) -> Cat(rs_data === rt_data, N, I),
    (fu_op === BR_NE) -> Cat(rs_data =/= rt_data, N, I),
    (fu_op === BR_LEZ) -> Cat(rs_data.asSInt <= 0.S, N, I),
    (fu_op === BR_GTZ) -> Cat(rs_data.asSInt > 0.S, N, I),
    (fu_op === BR_LTZ) -> Cat(rs_data.asSInt < 0.S, N, I),
    (fu_op === BR_J) -> Cat(true.B, N, J),
    (fu_op === BR_JAL) -> Cat(true.B, Y, J),
    (fu_op === BR_JR) -> Cat(true.B, N, JR),
    (fu_op === BR_JALR) -> Cat(true.B, Y, JR),
    ));

  io.isu.ready := !in_stage_1 || io.wbu.ready;

  // bypass signals
  io.bypass.valid := io.wbu.valid;
  io.bypass.bits.wen := io.wbu.bits.need_wb;
  io.bypass.bits.reg_dest_idx := io.wbu.bits.reg_dest_idx;
  io.bypass.bits.data := io.wbu.bits.data;

  // wbu signals
  val mux_result = new MR(mux_result_u);
  io.wbu.bits.npc := datain.npc;
  io.wbu.bits.need_br := mux_result.need_br;
  io.wbu.bits.br_target := mux_result.br_target;
  io.wbu.bits.need_wb := mux_result.need_wb;
  io.wbu.bits.data := npc + 4.U;
  io.wbu.bits.reg_dest_idx := Mux(fu_op === BR_JAL, 31.U, reg_dest_idx);
  io.wbu.valid := in_stage_1 && !io.flush.valid;
}

