package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class IDU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val ifu = Flipped(DecoupledIO(new IFU_IDU_IO));
    val isu = DecoupledIO(new IDU_ISU_IO);
    val flush = Flipped(ValidIO(new FlushIO));
  });

  val fu_in = RegEnable(next=io.ifu.bits, enable=io.ifu.fire())
  val fu_valid = RegInit(N)

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
  ));

  val (valid: Bool) :: fu_type :: fu_op :: op1_sel :: op2_sel :: dest_sel :: Nil = csignals;


  io.ifu.ready := io.isu.ready || !fu_valid;

  // ISU
  io.isu.valid := fu_valid
  io.isu.bits.npc := npc;
  io.isu.bits.instr := instr;
  io.isu.bits.fu_type := fu_type;
  io.isu.bits.fu_op := fu_op;
  io.isu.bits.op1_sel := op1_sel;
  io.isu.bits.op2_sel := op2_sel;
  io.isu.bits.dest_sel := dest_sel;

  when (io.flush.valid || (!io.ifu.fire() && io.isu.fire())) {
    fu_valid := N
  } .elsewhen(!io.flush.valid && io.ifu.fire()) {
    fu_valid := Y
  }
}

