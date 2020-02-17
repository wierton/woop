package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._

class RegSet[T<:Data](n:Int, unit:T, zero:Boolean=true) {
  private val rf = Mem(n, unit);
  private val w = unit.getWidth;
  def isZero(addr:UInt) = if(zero) addr === 0.U else false.B;

  def read(addr:Int):T = {
    require(0 <= addr && addr < n);
    if(addr == 0) 0.U(w.W).asInstanceOf[T] else rf(addr);
  }

  def read(addr:UInt):T = {
    require(addr.getWidth == log2Up(n));
    Mux(isZero(addr), 0.U(w.W).asInstanceOf[T], rf(addr));
  }

  def write(addr:UInt, data:UInt):Unit = {
    require(data.getWidth == unit.getWidth);
    require(addr.getWidth == log2Up(n));
    rf(addr) := Mux(isZero(addr), 0.U, data);
  }
}

class ISU extends Module {
  val io = IO(new Bundle {
    val wbu = Flipped(ValidIO(new WriteBackIO));
    val idu = Flipped(DecoupledIO(new IDU_ISU_IO));
    val alu_bypass = Flipped(ValidIO(new BypassIO));
    val mdu_bypass = Flipped(ValidIO(new BypassIO));
    val lsu_bypass = Flipped(ValidIO(new BypassIO));
    val bru_bypass = Flipped(ValidIO(new BypassIO));
    val cmovu_bypass = Flipped(ValidIO(new BypassIO));
    val alu = DecoupledIO(new ISU_ALU_IO);
    val mdu = DecoupledIO(new ISU_MDU_IO);
    val lsu = DecoupledIO(new ISU_LSU_IO);
    val bru = DecoupledIO(new ISU_BRU_IO);
    val cmovu = DecoupledIO(new ISU_CMOVU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val sb = new RegSet(conf.xprlen, Bool());
  val rf = new RegSet(conf.xprlen, UInt(conf.xprlen.W));

  val flush_valid = io.flush.valid;
  val idu_valid = RegNext(next=io.idu.fire(), init=false.B);
  val in_stage_1 = idu_valid;
  val datain = RegEnable(next=io.idu.bits, enable=io.idu.fire());
  val instr = new Instr(datain.instr);
  val fu_type = datain.fu_type;
  val fu_op = datain.fu_op;
  val op1_sel = datain.op1_sel;
  val op2_sel = datain.op2_sel;
  val dest_sel = datain.dest_sel;

  import ExtOperation._
  val shamt_ext = instr.shamt.ZExt(conf.xprlen);
  val se_imm = instr.imm.SExt(conf.xprlen);
  val ze_imm = instr.imm.ZExt(conf.xprlen);
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W));


  // ======================
  // bypass, wb, rf
  // ======================

  // bypass
  val bypass_valids = Seq(io.alu_bypass.valid, io.mdu_bypass.valid, io.lsu_bypass.valid, io.bru_bypass.valid, io.cmovu_bypass.valid);
  val bypass_valid = Cat(bypass_valids).orR;
  assert(AtMost1H(bypass_valids:_*));
  val bypass_need_wb = Mux1H(Array(
    io.alu_bypass.valid -> io.alu_bypass.bits.wen,
    io.mdu_bypass.valid -> io.mdu_bypass.bits.wen,
    io.lsu_bypass.valid -> io.lsu_bypass.bits.wen,
    io.bru_bypass.valid -> io.bru_bypass.bits.wen,
    io.cmovu_bypass.valid -> io.cmovu_bypass.bits.wen
  ));
  val bypass_reg_dest_idx = Mux1H(Array(
    io.alu_bypass.valid -> io.alu_bypass.bits.reg_dest_idx,
    io.mdu_bypass.valid -> io.mdu_bypass.bits.reg_dest_idx,
    io.lsu_bypass.valid -> io.lsu_bypass.bits.reg_dest_idx,
    io.bru_bypass.valid -> io.bru_bypass.bits.reg_dest_idx,
    io.cmovu_bypass.valid -> io.cmovu_bypass.bits.reg_dest_idx
  ));
  val bypass_data = Mux1H(Array(
    io.alu_bypass.valid -> io.alu_bypass.bits.data,
    io.mdu_bypass.valid -> io.mdu_bypass.bits.data,
    io.lsu_bypass.valid -> io.lsu_bypass.bits.data,
    io.bru_bypass.valid -> io.bru_bypass.bits.data,
    io.cmovu_bypass.valid -> io.cmovu_bypass.bits.data
  ));


  // wb
  val wb_valid = io.wbu.valid;
  val wb_need_wb = io.wbu.bits.wen;
  val wb_data = io.wbu.bits.data;

  when(io.idu.fire()) {
    log("[ISU] [CPC] >>>>>> %x <<<<<<\n", io.idu.bits.npc - 4.U);
  }

  // rf
  case class R_REG(ready:Bool, data:UInt);
  def safe_read(idx:UInt) = {
    val is_zero = idx === 0.U;
    val reg_match = !sb.read(idx) || (bypass_valid && !bypass_need_wb) || (wb_valid && !wb_need_wb);
    val bypass_match = bypass_valid && bypass_need_wb && bypass_reg_dest_idx === idx;
    val wb_match = wb_valid && wb_need_wb && io.wbu.bits.reg_dest_idx === idx;
    val ready = is_zero || reg_match || bypass_match || wb_match;
    val reg_data = rf.read(idx);
    val data = MuxCase(0.U, Array(
      (is_zero) -> 0.U,
      (reg_match) -> reg_data,
      (bypass_match) -> bypass_data,
      (wb_match) -> wb_data,
      ));

    when(io.idu.fire() && ready) {
      log("[ISU] [SR] sb.read(%x) = %x\n", idx, sb.read(idx));
      log("[ISU] [SR] bypass_need_wb:%x, bypass_valid:%x\n", bypass_valid, bypass_need_wb);
      log("[ISU] [SR] wbu_need_wb:%x, wb_valid:%x\n", wb_valid, wb_need_wb);
      log("[ISU] [SR:%x] is_zero:%x, reg_match:%x, bypass_match:%x, wb_match:%x\n", idx, is_zero, reg_match, bypass_match, wb_match);
    }

    new R_REG(ready, data);
  }

  val op1_idx = Mux1H(Array(
    (op1_sel === OP1_RS) -> instr.rs_idx,
    (op1_sel === OP1_RT) -> instr.rt_idx,
    (op1_sel === OP1_IMU) -> 0.U
  )).asUInt;

  val op2_idx = Mux1H(Array(
    (op2_sel === OP2_RS)  -> instr.rs_idx,
    (op2_sel === OP2_RT)  -> instr.rt_idx,
    )).asUInt;

  // read data
  val op1_reg = safe_read(op1_idx);
  val op2_reg = safe_read(op2_idx);

  when(op1_reg.ready || op2_reg.ready) {
    log("[ISU] op1_reg:%x, op2_reg:%x\n", op1_reg.data, op2_reg.data);
  }

  // caculate op
  val op1 = Mux1H(Array(
    (op1_sel === OP1_RS) -> op1_reg.data,
    (op1_sel === OP1_RT) -> op1_reg.data,
    (op1_sel === OP1_IMU) -> ue_imm
  )).asUInt;

  val op2 = Mux1H(Array(
    (op2_sel === OP2_RS)  -> op2_reg.data,
    (op2_sel === OP2_RT)  -> op2_reg.data,
    (op2_sel === OP2_IMI) -> se_imm,
    (op2_sel === OP2_IMZ) -> ze_imm,
    (op2_sel === OP2_SA)  -> shamt_ext
  )).asUInt;

  val reg_dest_idx = Mux1H(Array(
    (dest_sel === DEST_RD) -> instr.rd_idx,
    (dest_sel === DEST_RT) -> instr.rt_idx
  )).asUInt;

  val isu_valid = in_stage_1 && op1_reg.ready && op2_reg.ready && !io.flush.valid;
  val fu_ready = io.alu.ready && io.mdu.ready && io.bru.ready && io.lsu.ready && io.cmovu.ready;
  val outfire = io.alu.fire() || io.mdu.fire() || io.bru.fire() || io.lsu.fire() || io.cmovu.fire();

  io.idu.ready := !flush_valid && (!in_stage_1 || fu_ready);

  // ALU IO
  io.alu.bits.fu_op := fu_op;
  io.alu.bits.npc := datain.npc;
  io.alu.bits.op1 := op1;
  io.alu.bits.op2 := op2;
  io.alu.bits.reg_dest_idx := reg_dest_idx;
  io.alu.valid := isu_valid && fu_ready && fu_type === FU_ALU;

  // MDU IO
  io.mdu.bits.fu_op := fu_op;
  io.mdu.bits.npc := datain.npc;
  io.mdu.bits.op1 := op1;
  io.mdu.bits.op2 := op2;
  io.mdu.bits.reg_dest_idx := reg_dest_idx;
  io.mdu.valid := isu_valid && fu_ready && fu_type === FU_MDU;

  // LSU IO
  io.lsu.bits.fu_op := fu_op;
  io.lsu.bits.npc := datain.npc;
  io.lsu.bits.base := op1;
  io.lsu.bits.offset := se_imm;
  io.lsu.bits.data := op2;
  io.lsu.bits.reg_dest_idx := reg_dest_idx;
  io.lsu.valid := isu_valid && fu_ready && fu_type === FU_LSU;

  // BRU IO
  io.bru.bits.fu_op := fu_op;
  io.bru.bits.npc := datain.npc;
  io.bru.bits.rs_data := op1;
  io.bru.bits.rt_data := op2;
  io.bru.bits.addr := instr.addr;
  io.bru.bits.se_off := se_imm;
  io.bru.bits.reg_dest_idx := reg_dest_idx;
  io.bru.valid := isu_valid && fu_ready && fu_type === FU_BRU;

  // CMOVU
  io.cmovu.bits.fu_op := fu_op;
  io.cmovu.bits.npc := datain.npc;
  io.cmovu.bits.op1 := op1;
  io.cmovu.bits.op2 := op2;
  io.cmovu.bits.reg_dest_idx := reg_dest_idx;
  io.cmovu.valid := isu_valid && fu_ready && fu_type === FU_CMOVU;

  when(io.wbu.valid && io.wbu.bits.wen) {
    log("[ISU] rf.write(%x, %x)\n", io.wbu.bits.reg_dest_idx, wb_data);
    rf.write(io.wbu.bits.reg_dest_idx, wb_data);
  }

  when(io.wbu.valid && reg_dest_idx != io.wbu.bits.reg_dest_idx) {
    log("[ISU] wbvalid, sb.write(%x, false)\n", io.wbu.bits.reg_dest_idx);
    sb.write(io.wbu.bits.reg_dest_idx, false.B); // clear tag
  }

  when(outfire) {
    log("[ISU] outfire, sb.write(%x, true), npc:%x\n", reg_dest_idx, datain.npc);
    sb.write(reg_dest_idx, true.B);
  }

  when(io.idu.fire()) {
    log("[ISU] <- [IDU] idu fire\n");
  }

  when(io.wbu.valid) {
    log("[ISU] wb: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.wbu.valid, io.wbu.bits.wen, io.wbu.bits.reg_dest_idx, io.wbu.bits.data);
  }
  when(io.alu_bypass.valid) {
    log("[ISU] alu_bypass: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.alu_bypass.valid, io.alu_bypass.bits.wen, io.alu_bypass.bits.reg_dest_idx, io.alu_bypass.bits.data);
  }
  when(io.mdu_bypass.valid) {
    log("[ISU] mdu_bypass: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.mdu_bypass.valid, io.mdu_bypass.bits.wen, io.mdu_bypass.bits.reg_dest_idx, io.mdu_bypass.bits.data);
  }
  when(io.bru_bypass.valid) {
    log("[ISU] bru_bypass: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.bru_bypass.valid, io.bru_bypass.bits.wen, io.bru_bypass.bits.reg_dest_idx, io.bru_bypass.bits.data);
  }
  when(io.lsu_bypass.valid) {
    log("[ISU] lsu_bypass: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.lsu_bypass.valid, io.lsu_bypass.bits.wen, io.lsu_bypass.bits.reg_dest_idx, io.lsu_bypass.bits.data);
  }
  when(io.cmovu_bypass.valid) {
    log("[ISU] cmovu_bypass: .valid=%x, .wen=%x, .rd=%x, .data=%x\n", io.cmovu_bypass.valid, io.cmovu_bypass.bits.wen, io.cmovu_bypass.bits.reg_dest_idx, io.cmovu_bypass.bits.data);
  }

  // output some log
  when(outfire) {
    log("[ISU] -> [FU] npc:%x, instr:%x\n", io.idu.bits.npc, io.idu.bits.instr);
    log("[ISU] -> [FU] se_imm:%x, ze_imm:%x, ue_imm:%x\n", se_imm, ze_imm, ue_imm);
    log("[ISU] -> [FU] fu_type:%x, fu_op:%x\n", io.idu.bits.fu_type, io.idu.bits.fu_op);
    log("[ISU] -> [FU] op1_sel:%x, op2_sel:%x\n", io.idu.bits.op1_sel, io.idu.bits.op2_sel);
    log("[ISU] -> [FU] dest_sel:%x\n", io.idu.bits.dest_sel);
  }

  when(io.alu.fire() && fu_type === FU_ALU) {
    log("[ISU] -> [ALU] fu_op:%x, op1:%x, op2:%x, reg_dest_idx:%x\n", io.alu.bits.fu_op, io.alu.bits.op1, io.alu.bits.op2, io.alu.bits.reg_dest_idx);
  }

  // commit
  when(io.wbu.valid) {
    log("[ISU] wb.valid, wen:%x, addr:%x, data:%x\n", io.wbu.bits.wen, io.wbu.bits.reg_dest_idx, io.wbu.bits.data);
    printf("$0 :0x%x  $at:0x%x  $v0:0x%x  $v1:0x%x\n", rf.read(0), rf.read(1), rf.read(2), rf.read(3));
    printf("$a0:0x%x  $a1:0x%x  $a2:0x%x  $a3:0x%x\n", rf.read(4), rf.read(5), rf.read(6), rf.read(7));
    printf("$t0:0x%x  $t1:0x%x  $t2:0x%x  $t3:0x%x\n", rf.read(8), rf.read(9), rf.read(10), rf.read(11));
    printf("$t4:0x%x  $t5:0x%x  $t6:0x%x  $t7:0x%x\n", rf.read(12), rf.read(13), rf.read(14), rf.read(15));
    printf("$s0:0x%x  $s1:0x%x  $s2:0x%x  $s3:0x%x\n", rf.read(16), rf.read(17), rf.read(18), rf.read(19));
    printf("$s4:0x%x  $s5:0x%x  $s6:0x%x  $s7:0x%x\n", rf.read(20), rf.read(21), rf.read(22), rf.read(23));
    printf("$t8:0x%x  $t9:0x%x  $k0:0x%x  $k1:0x%x\n", rf.read(24), rf.read(25), rf.read(26), rf.read(27));
    printf("$gp:0x%x  $sp:0x%x  $fp:0x%x  $ra:0x%x\n", rf.read(28), rf.read(29), rf.read(30), rf.read(31));
    printf("pc: 0x%x\n", io.wbu.bits.npc - 4.U);
  }
}

