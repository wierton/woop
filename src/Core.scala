package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._


class Instr {
  val op     = Wire(UInt(OP_SZ.W));
  val rs_idx = Wire(UInt(REG_SZ.W));
  val rt_idx = Wire(UInt(REG_SZ.W));
  val rd_idx = Wire(UInt(REG_SZ.W));
  val shamt  = Wire(UInt(SHAMT_SZ.W));
  val func   = Wire(UInt(FUNC_SZ.W));

  def imm    = Cat(rd_idx, shamt, func);
  def addr   = Cat(rs_idx, rt_idx, imm);

  private val SeqAll = Seq(op, rs_idx, rt_idx, rd_idx, shamt, func);

  def this(in:UInt) {
    this();
    Tie(SeqAll:_*) := in;
  }

  def := (in:UInt):Unit = {
    Tie(SeqAll:_*) := in;
  }

  def asUInt() = Cat(SeqAll);
}

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

class IFU extends Module {
  val io = IO(new Bundle {
    val mem = new MemoryIO;
    val idu = DecoupledIO(new IFU_IDU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  // init to be valid, the first instruction
  val pc = Reg(UInt(conf.xprlen.W), init=conf.start_addr);
  val pc_next = pc + 4.U(conf.xprlen.W);

  val req_fire = io.mem.req.fire();
  val resp_fire = io.mem.resp.fire();
  val instr = io.mem.resp.bits.data;

  io.mem.req.valid := io.idu.ready && !io.flush.valid;
  io.mem.req.bits.addr  := pc - MEM_ST.U;
  io.mem.req.bits.func  := MX_RD;
  io.mem.req.bits.dtype := DT_W;
  io.mem.req.bits.data  := 0.U;

  when(req_fire) {
    pc := pc_next;
  }

  when(io.flush.valid) { pc := io.flush.bits.br_target; }

  io.mem.resp.ready := true.B;

  io.idu.valid := io.mem.resp.valid && !io.flush.valid;
  io.idu.bits.npc := pc;
  io.idu.bits.instr := instr;

  // print some logs
  when(io.mem.req.fire()) {
    log("[IFU] [CPC] >>>>>> %x <<<<<<\n", pc);
  }
  when(io.flush.valid) {
    log("[IFU] get flush signal, br to %x\n", io.flush.bits.br_target);
  }
  when(io.mem.req.valid || io.mem.req.ready) {
    log("[IFU] mem.req: ready=%x, valid=%x, addr=%x, pc=%x\n", io.mem.req.ready, io.mem.req.valid, io.mem.req.bits.addr, pc);
  }
  when(io.mem.resp.valid || io.mem.resp.ready) {
    log("[IFU] mem.resp: ready=%x, valid=%x, data=%x\n", io.mem.resp.ready, io.mem.resp.valid, io.mem.resp.bits.data);
  }
}

class IDU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val ifu = Flipped(DecoupledIO(new IFU_IDU_IO));
    val isu = DecoupledIO(new IDU_ISU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
    val gpio = new IDU_GPIO_IO;
  });

  val ifu_fire = io.ifu.fire();
  val instr_valid = RegNext(next=ifu_fire, init=false.B);
  val datain = RegEnable(next=io.ifu.bits, enable=ifu_fire);
  val npc = datain.npc;
  val instr = datain.instr;
  val in_stage_1 = instr_valid;

  // instruction decode stage
  val csignals = ListLookup(instr,
    List(N, FU_X, FU_OP_X, OP1_X, OP2_X, DEST_X), Array(
      /* instr | fu_type  |  fu_op  |  op1_sel  |  op2_sel |  dest_sel */
     // ALU instructions
     LUI     -> List(Y, FU_ALU,  ALU_COPY1,  OP1_IMU,  OP2_X,   DEST_RT),
     ADD     -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_RT,  DEST_RD),
     ADDU    -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_RT,  DEST_RD),
     SUB     -> List(Y, FU_ALU,  ALU_SUB,    OP1_RS,   OP2_RT,  DEST_RD),
     SUBU    -> List(Y, FU_ALU,  ALU_SUB,    OP1_RS,   OP2_RT,  DEST_RD),
     SLT     -> List(Y, FU_ALU,  ALU_SLT,    OP1_RS,   OP2_RT,  DEST_RD),
     SLTU    -> List(Y, FU_ALU,  ALU_SLTU,   OP1_RS,   OP2_RT,  DEST_RD),
     AND     -> List(Y, FU_ALU,  ALU_AND,    OP1_RS,   OP2_RT,  DEST_RD),
     OR      -> List(Y, FU_ALU,  ALU_OR,     OP1_RS,   OP2_RT,  DEST_RD),
     XOR     -> List(Y, FU_ALU,  ALU_XOR,    OP1_RS,   OP2_RT,  DEST_RD),
     NOR     -> List(Y, FU_ALU,  ALU_NOR,    OP1_RS,   OP2_RT,  DEST_RD),
     SLTI    -> List(Y, FU_ALU,  ALU_SLT,    OP1_RS,   OP2_IMI, DEST_RT),
     SLTIU   -> List(Y, FU_ALU,  ALU_SLTU,   OP1_RS,   OP2_IMI, DEST_RT),
     SLL     -> List(Y, FU_ALU,  ALU_SLL,    OP1_RT,   OP2_SA,  DEST_RD),
     SRA     -> List(Y, FU_ALU,  ALU_SRA,    OP1_RT,   OP2_SA,  DEST_RD),
     SRL     -> List(Y, FU_ALU,  ALU_SRL,    OP1_RT,   OP2_SA,  DEST_RD),
     SRAV    -> List(Y, FU_ALU,  ALU_SRA,    OP1_RT,   OP2_RS,  DEST_RD),
     SRLV    -> List(Y, FU_ALU,  ALU_SRL,    OP1_RT,   OP2_RS,  DEST_RD),
     SLLV    -> List(Y, FU_ALU,  ALU_SLL,    OP1_RT,   OP2_RS,  DEST_RD),
     ADDI    -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_IMI, DEST_RT),
     ADDIU   -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_IMI, DEST_RT),
     ANDI    -> List(Y, FU_ALU,  ALU_AND,    OP1_RS,   OP2_IMZ, DEST_RT),
     ORI     -> List(Y, FU_ALU,  ALU_OR,     OP1_RS,   OP2_IMZ, DEST_RT),
     XORI    -> List(Y, FU_ALU,  ALU_XOR,    OP1_RS,   OP2_IMZ, DEST_RT),

     // BRU instructions
     BEQ     -> List(Y, FU_BRU,  BR_EQ,      OP1_RS,   OP2_RT,  DEST_RD),
     BNE     -> List(Y, FU_BRU,  BR_NE,      OP1_RS,   OP2_RT,  DEST_RD),
     BLEZ    -> List(Y, FU_BRU,  BR_LEZ,     OP1_RS,   OP2_X,   DEST_RD),
     BGTZ    -> List(Y, FU_BRU,  BR_GTZ,     OP1_RS,   OP2_X,   DEST_RD),
     BLTZ    -> List(Y, FU_BRU,  BR_LTZ,     OP1_RS,   OP2_X,   DEST_RD),
     J       -> List(Y, FU_BRU,  BR_J,       OP1_RS,   OP2_X,   DEST_RD),
     JAL     -> List(Y, FU_BRU,  BR_JAL,     OP1_RS,   OP2_X,   DEST_RD),
     JR      -> List(Y, FU_BRU,  BR_JR,      OP1_RS,   OP2_X,   DEST_RD),
     JALR    -> List(Y, FU_BRU,  BR_JALR,    OP1_RS,   OP2_X,   DEST_RD),

     // LSU instructions
     LW      -> List(Y, FU_LSU,  LSU_LW,     OP1_X,    OP2_X,   DEST_RT),
     SW      -> List(Y, FU_LSU,  LSU_SW,     OP1_X,    OP2_X,   DEST_X),
     LB      -> List(Y, FU_LSU,  LSU_LB,     OP1_X,    OP2_X,   DEST_RT),
     LBU     -> List(Y, FU_LSU,  LSU_LBU,    OP1_X,    OP2_X,   DEST_RT),
     SB      -> List(Y, FU_LSU,  LSU_SB,     OP1_X,    OP2_X,   DEST_X),
     LH      -> List(Y, FU_LSU,  LSU_LH,     OP1_X,    OP2_X,   DEST_RT),
     LHU     -> List(Y, FU_LSU,  LSU_LHU,    OP1_X,    OP2_X,   DEST_RT),
     SH      -> List(Y, FU_LSU,  LSU_SH,     OP1_X,    OP2_X,   DEST_X),
     LWL     -> List(Y, FU_LSU,  LSU_LWL,    OP1_X,    OP2_X,   DEST_RT),
     LWR     -> List(Y, FU_LSU,  LSU_LWR,    OP1_X,    OP2_X,   DEST_RT),
     SWL     -> List(Y, FU_LSU,  LSU_SWL,    OP1_X,    OP2_X,   DEST_RT),
     SWR     -> List(Y, FU_LSU,  LSU_SWR,    OP1_X,    OP2_X,   DEST_RT),

     // MDU instructions
     MFHI    -> List(Y, FU_MDU,  MDU_MFHI,   OP1_X,    OP2_X,   DEST_RD),
     MFLO    -> List(Y, FU_MDU,  MDU_MFLO,   OP1_X,    OP2_X,   DEST_RD),
     MUL     -> List(Y, FU_MDU,  MDU_MUL,    OP1_RS,   OP2_RT,  DEST_RD),
     MULT    -> List(Y, FU_MDU,  MDU_MULT,   OP1_RS,   OP2_RT,  DEST_X),
     MULTU   -> List(Y, FU_MDU,  MDU_MULTU,  OP1_RS,   OP2_RT,  DEST_X),
     DIV     -> List(Y, FU_MDU,  MDU_DIV,    OP1_RS,   OP2_RT,  DEST_X),
     DIVU    -> List(Y, FU_MDU,  MDU_DIVU,   OP1_RS,   OP2_RT,  DEST_X),

     // CMOVU instructions
     MOVN    -> List(Y, FU_CMOVU,CMOVU_MOVN, OP1_RS,   OP2_RT,   DEST_RD),
     MOVZ    -> List(Y, FU_CMOVU,CMOVU_MOVZ, OP1_RS,   OP2_RT,   DEST_RD)
  ));

  val (valid: Bool) :: fu_type :: fu_op :: op1_sel :: op2_sel :: dest_sel :: Nil = csignals;


  io.gpio.invalid_instr := !valid;
  io.gpio.npc := npc;
  io.gpio.instr := instr;

  //               not busy        pipelined
  io.ifu.ready := !in_stage_1 || io.isu.ready;

  // ISU
  io.isu.valid := valid && in_stage_1 && !io.flush.valid;
  io.isu.bits.npc := npc;
  io.isu.bits.instr := instr;
  io.isu.bits.fu_type := fu_type;
  io.isu.bits.fu_op := fu_op;
  io.isu.bits.op1_sel := op1_sel;
  io.isu.bits.op2_sel := op2_sel;
  io.isu.bits.dest_sel := dest_sel;

  when(!valid) {
    log("[IDU] Invalid instr %x\n", datain.instr);
  }

  when(ifu_fire) {
    log("[IDU] [CPC] >>>>>> %x <<<<<<\n", io.ifu.bits.npc - 4.U);
    log("[IDU] input_valid:instr=%x\n", io.ifu.bits.instr);
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

class LSU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val mem = new MemoryIO;
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
  io.mem.req.bits.dtype := Mux(lsuop.isAlign(), lsuop.dt, u_dt);
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

class MDU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val bypass = ValidIO(new BypassIO);
    val isu = Flipped(DecoupledIO(new ISU_MDU_IO));
    val wbu = DecoupledIO(new MDU_WBU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val datain = RegEnable(next=io.isu.bits, enable=io.isu.fire());
  val isu_valid = RegNext(next=io.isu.fire(), init=false.B);
  val in_stage_1 = isu_valid;
  val fu_op = datain.fu_op;
  val op1 = datain.op1;
  val op2 = datain.op2;
  val reg_dest_idx = datain.reg_dest_idx;

  val hi = RegInit(0.U(conf.xprlen.W));
  val lo = RegInit(0.U(conf.xprlen.W));

  val whi = WireInit(0.U(conf.xprlen.W));
  val wlo = WireInit(0.U(conf.xprlen.W));

  val mduop = Wire(new MDUOp());
  mduop := fu_op;

  io.isu.ready := !in_stage_1 || io.wbu.ready;

  io.bypass.valid := io.wbu.valid;
  io.bypass.bits.wen := io.wbu.bits.need_wb;
  io.bypass.bits.reg_dest_idx := io.wbu.bits.reg_dest_idx;
  io.bypass.bits.data := io.wbu.bits.data;

  io.wbu.bits.npc := datain.npc;
  io.wbu.bits.data := wlo;
  io.wbu.bits.need_wb := mduop.wb_reg === WB_RD;
  io.wbu.bits.reg_dest_idx := reg_dest_idx;
  io.wbu.valid := in_stage_1 && !io.flush.valid;

  when(io.isu.fire()) {
    log("[MDU] fu_op:%x, op1:%x, op2:%x\n", fu_op, op1, op2);
    log("[MDU] reg_dest_idx:%x\n", reg_dest_idx);
    log("[MDU] mf_reg:%x, fcn:%x\n", mduop.mf_reg, mduop.func);
    log("[MDU] sign:%x, wb_reg:%x\n", mduop.sign, mduop.wb_reg);
  }

  when(io.wbu.fire()) {
    log("[MDU] hi:%x, lo:%x\n", hi, lo);
    log("[MDU] need_wb:%x\n", io.wbu.bits.need_wb);
    log("[MDU] wb_data:%x\n", io.wbu.bits.data);
  }

  when(mduop.isMul()) {
    val result = Wire(UInt((2 * conf.xprlen).W));
    when(mduop.isSigned()) {
      result := (op1.asSInt * op2.asSInt).asUInt;
      } .otherwise {
        result := op1.asUInt * op2.asUInt;
      }

      whi := result(2 * conf.xprlen - 1, conf.xprlen);
      wlo := result(conf.xprlen - 1, 0);
      } .elsewhen(mduop.isDiv()) {
        when(mduop.isSigned()) {
          whi := (op1.asSInt % op2.asSInt).asUInt;
          wlo := (op1.asSInt / op2.asSInt).asUInt;
          } .otherwise {
            whi := op1.asUInt % op2.asUInt;
            wlo := op1.asUInt / op2.asUInt;
          }
          } .otherwise {
            when(mduop.mf_reg === MF_LO) {
              io.wbu.bits.data := lo;
              } .otherwise {
                io.wbu.bits.data := hi;
              }
          }

          when(io.isu.fire() && mduop.wb_reg === WB_HL) {
            hi := whi;
            lo := wlo;
          }

          when(io.isu.fire()) {
            log("[MDU] [CPC] >>>>>> %x <<<<<<\n", io.isu.bits.npc - 4.U);
          }
}

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

class WBU extends Module {
  val io = IO(new Bundle {
    val alu = Flipped(DecoupledIO(new ALU_WBU_IO));
    val mdu = Flipped(DecoupledIO(new MDU_WBU_IO));
    val lsu = Flipped(DecoupledIO(new LSU_WBU_IO));
    val bru = Flipped(DecoupledIO(new BRU_WBU_IO));
    val cmovu = Flipped(DecoupledIO(new CMOVU_WBU_IO));
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
  val cmovu_valid = RegNext(next=io.cmovu.fire(), init=false.B);

  // flush_valid
  val flush_valid = RegEnable(next=io.bru.bits.need_br, init=false.B, enable=io.bru.fire());

  // fu_valid
  val fu_valid = alu_valid || mdu_valid || lsu_valid || bru_valid || cmovu_valid;

  assert(AtMost1H(io.alu.valid, io.mdu.valid, io.lsu.valid, io.bru.valid, io.cmovu.valid));

  io.alu.ready := true.B;
  io.mdu.ready := true.B;
  io.lsu.ready := true.B;
  io.bru.ready := true.B;
  io.cmovu.ready := true.B;

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

  when(io.cmovu.fire()) {
    log("[WBU] [CPC] >>>>>> %x <<<<<<\n", io.cmovu.bits.npc - 4.U);
    log("[WBU] <- [CMOVU]: addr:%x, data:%x\n", io.cmovu.bits.reg_dest_idx, io.cmovu.bits.data);
    wb_npc := io.cmovu.bits.npc;
    wb_addr := io.cmovu.bits.reg_dest_idx;
    wb_data := io.cmovu.bits.data;
    wb_wen := io.cmovu.bits.need_wb;
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

class Core extends Module {
  val io = IO(new Bundle {
    val out = new GPIO_OUT;
  });

  val serial = Module(new SimulatedSerial);
  serial.io.reset := reset;
  serial.io.clock := clock;

  val gpio = Module(new GPIO);
  io.out <> gpio.io.out;

  val testio = Module(new TestMappedIO);
  val memarb = Module(new AsyncMemoryArbiter(2, 1));

  val mmio = Seq(memarb, gpio, serial, testio);
  val crossbar = Module(new AsyncCrossbar(mmio:_*));
  crossbar.connect();

  val mem = Module(new SimulatedMemory(memarb.idwidth));
  mem.io.reset := reset;
  mem.io.clock := clock;

  val ifu = Module(new IFU);
  val idu = Module(new IDU);
  val isu = Module(new ISU); // rf
  val lsu = Module(new LSU);
  val alu = Module(new ALU);
  val mdu = Module(new MDU);
  val bru = Module(new BRU);
  val wbu = Module(new WBU); // rf
  val cmovu = Module(new CMOVU);

  // bypass signals
  isu.io.alu_bypass <> alu.io.bypass;
  isu.io.mdu_bypass <> mdu.io.bypass;
  isu.io.lsu_bypass <> lsu.io.bypass;
  isu.io.bru_bypass <> bru.io.bypass;
  isu.io.cmovu_bypass <> cmovu.io.bypass;

  // Flush signals
  ifu.io.flush <> wbu.io.flush;
  idu.io.flush <> wbu.io.flush;
  isu.io.flush <> wbu.io.flush;
  lsu.io.flush <> wbu.io.flush;
  alu.io.flush <> wbu.io.flush;
  mdu.io.flush <> wbu.io.flush;
  bru.io.flush <> wbu.io.flush;
  cmovu.io.flush <> wbu.io.flush;

  // GPIO
  idu.io.gpio <> gpio.io.idu;

  memarb.io.out <> mem.io.mem;
  ifu.io.mem <> memarb.io.in(0);
  lsu.io.mem <> crossbar.io.in;

  ifu.io.idu <> idu.io.ifu;
  idu.io.isu <> isu.io.idu;

  // ISU -> XXX
  isu.io.alu <> alu.io.isu;
  isu.io.mdu <> mdu.io.isu;
  isu.io.lsu <> lsu.io.isu;
  isu.io.bru <> bru.io.isu;
  isu.io.cmovu <> cmovu.io.isu;

  // XXX -> WBU
  lsu.io.wbu <> wbu.io.lsu;
  alu.io.wbu <> wbu.io.alu;
  mdu.io.wbu <> wbu.io.mdu;
  bru.io.wbu <> wbu.io.bru;
  cmovu.io.wbu <> wbu.io.cmovu;

  isu.io.wbu <> wbu.io.wb;

  // some logs
  when(gpio.io.out.halted) { log("[TOP] halted\n"); }
}

object Main extends App {
  chisel3.Driver.execute(args, () => new Core);
}
