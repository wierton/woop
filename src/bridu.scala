package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class BRIDU extends Module {
  val io = IO(new Bundle {
    val ifu = Flipped(DecoupledIO(new IFU_BRIDU_IO))
    val pralu = DecoupledIO(new BRIDU_PRALU_IO)
    val rs_idx = Output(REG_SZ.W)
    val rt_idx = Output(REG_SZ.W)
    val rs_data = Flipped(ValidIO(Output(conf.xprlen.W)))
    val rt_data = Flipped(ValidIO(Output(conf.xprlen.W)))
    val br_flush = ValidIO(new FlushIO)
  })

  val fu_in = RegEnable(next=io.ifu.bits, enable=io.ifu.fire())
  val fu_valid = RegInit(N)

  // instruction decode stage
  val csignals = ListLookup(fu_in.instr,
    List(N, FU_X, FU_OP_X, OP1_X, OP2_X, DEST_X), Array(
      /* instr | fu_type  |  fu_op  |  op1_sel  |  op2_sel |  rd_sel */
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
  ))

  val (valid: Bool) :: fu_type :: fu_op :: op1_sel :: op2_sel :: rd_sel :: Nil = csignals

  val instr = fu_in.instr.asTypeOf(new Instr)
  /* pralu */
  io.pralu.bits.fu_type := fu_type
  io.pralu.bits.fu_op := fu_op
  io.pralu.bits.op1_sel := op1_sel
  io.pralu.bits.op2_sel := op2_sel
  io.pralu.bits.rd_sel := rd_sel

  /* register RW */
  io.rs_idx := instr.rs_idx
  io.rt_idx := instr.rt_idx

  /* branch check */
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val I = (fu_in.pc + (fu_in.se_imm << 2))(31, 0)
  val J = Cat(Seq(fu_in.pc(31, 28), instr.addr, 0.U(2.W)))
  val JR = io.rs_data.bits
  /* br_info={34:ready, 33:jump, 32:wb, 31..0:target} */
  val br_info = Mux1H(Array(
    (fu_op === BR_EQ) -> Cat(io.rs_data.valid && io.rt_data.valid, io.rs_data.bits === io.rt_data.bits, N, I),
    (fu_op === BR_NE) -> Cat(io.rs_data.valid && io.rt_data.valid, io.rs_data.bits =/= io.rt_data.bits, N, I),
    (fu_op === BR_LEZ) -> Cat(io.rs_data.valid, io.rs_data.bits.asSInt <= 0.S, N, I),
    (fu_op === BR_GTZ) -> Cat(io.rs_data.valid, io.rs_data.bits.asSInt > 0.S, N, I),
    (fu_op === BR_LTZ) -> Cat(io.rs_data.valid, io.rs_data.bits.asSInt < 0.S, N, I),
    (fu_op === BR_J) -> Cat(Y, Y, N, J),
    (fu_op === BR_JAL) -> Cat(Y, Y, Y, J),
    (fu_op === BR_JR) -> Cat(Y, Y, N, JR),
    (fu_op === BR_JALR) -> Cat(Y, Y, Y, JR)))

  io.ifu.ready := (io.pralu.ready && br_info(34)) || !fu_valid
  io.br_flush.valid := fu_valid && fu_type === FU_BRU && br_info(34) && br_info(33)
  io.br_flush.bits.br_target := br_info(31, 0)

  /* wb */
  io.pralu.valid := fu_valid
  io.pralu.bits.wb.pc := fu_in.pc
  io.pralu.bits.wb.instr := instr
  /* only valid for bru */
  io.pralu.bits.wb.rd_idx := Mux(rd_sel === DEST_RT, instr.rt_idx, instr.rd_idx)
  io.pralu.bits.wb.wen := br_info(32)
  io.pralu.bits.wb.data := fu_in.pc + 8.U
  /* only valid for bru */

  when (io.br_flush.valid || (!io.ifu.fire() && io.pralu.fire())) {
    fu_valid := N
  } .elsewhen(!io.br_flush.valid && io.ifu.fire()) {
    fu_valid := Y
  }

  if (conf.log_BRIDU) {
    printf("%d: BRIDU: fu_valid=%b\n", GTimer(), fu_valid)
    fu_in.dump("BRIDU.fu_in")
    io.ifu.dump("BRIDU.ifu")
    io.pralu.dump("BRIDU.pralu")
    io.br_flush.dump("BRIDU.flush")
  }
}

