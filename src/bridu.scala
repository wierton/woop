package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

class BRIDU extends Module with LSUConsts with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new IFU_BRIDU_IO))
    val fu_out = DecoupledIO(new BRIDU_PRALU_IO)
    val rfio = new RegFileIO
    val br_flush = ValidIO(new FlushIO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)

  // instruction decode stage
  val csignals = ListLookup(fu_in.instr,
    List(N, FU_X, FU_OP_X, OP1_X, OP2_X, OPD_X), Array(
      /* instr | fu_type  |  fu_op  |  op1_sel  |  op2_sel |  rd_sel */
     // ALU instructions
     LUI     -> List(Y, FU_ALU,  ALU_LUI,    OP1_X,    OP2_IMU, OPD_RT),
     ADD     -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_RT,  OPD_RD),
     ADDU    -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_RT,  OPD_RD),
     SUB     -> List(Y, FU_ALU,  ALU_SUB,    OP1_RS,   OP2_RT,  OPD_RD),
     SUBU    -> List(Y, FU_ALU,  ALU_SUB,    OP1_RS,   OP2_RT,  OPD_RD),
     SLT     -> List(Y, FU_ALU,  ALU_SLT,    OP1_RS,   OP2_RT,  OPD_RD),
     SLTU    -> List(Y, FU_ALU,  ALU_SLTU,   OP1_RS,   OP2_RT,  OPD_RD),
     AND     -> List(Y, FU_ALU,  ALU_AND,    OP1_RS,   OP2_RT,  OPD_RD),
     OR      -> List(Y, FU_ALU,  ALU_OR,     OP1_RS,   OP2_RT,  OPD_RD),
     XOR     -> List(Y, FU_ALU,  ALU_XOR,    OP1_RS,   OP2_RT,  OPD_RD),
     NOR     -> List(Y, FU_ALU,  ALU_NOR,    OP1_RS,   OP2_RT,  OPD_RD),
     SLTI    -> List(Y, FU_ALU,  ALU_SLT,    OP1_RS,   OP2_IMI, OPD_RT),
     SLTIU   -> List(Y, FU_ALU,  ALU_SLTU,   OP1_RS,   OP2_IMI, OPD_RT),
     SLL     -> List(Y, FU_ALU,  ALU_SLL,    OP1_RT,   OP2_SA,  OPD_RD),
     SRA     -> List(Y, FU_ALU,  ALU_SRA,    OP1_RT,   OP2_SA,  OPD_RD),
     SRL     -> List(Y, FU_ALU,  ALU_SRL,    OP1_RT,   OP2_SA,  OPD_RD),
     SRAV    -> List(Y, FU_ALU,  ALU_SRA,    OP1_RT,   OP2_RS,  OPD_RD),
     SRLV    -> List(Y, FU_ALU,  ALU_SRL,    OP1_RT,   OP2_RS,  OPD_RD),
     SLLV    -> List(Y, FU_ALU,  ALU_SLL,    OP1_RT,   OP2_RS,  OPD_RD),
     ADDI    -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_IMI, OPD_RT),
     ADDIU   -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_IMI, OPD_RT),
     ANDI    -> List(Y, FU_ALU,  ALU_AND,    OP1_RS,   OP2_IMZ, OPD_RT),
     ORI     -> List(Y, FU_ALU,  ALU_OR,     OP1_RS,   OP2_IMZ, OPD_RT),
     XORI    -> List(Y, FU_ALU,  ALU_XOR,    OP1_RS,   OP2_IMZ, OPD_RT),
     MOVN    -> List(Y, FU_ALU,  ALU_MOVN,   OP1_RS,   OP2_RT,  OPD_RD),
     MOVZ    -> List(Y, FU_ALU,  ALU_MOVZ,   OP1_RS,   OP2_RT,  OPD_RD),

     // BRU instructions
     BEQ     -> List(Y, FU_BRU,  BR_EQ,      OP1_RS,   OP2_RT,  OPD_X),
     BNE     -> List(Y, FU_BRU,  BR_NE,      OP1_RS,   OP2_RT,  OPD_X),
     BLEZ    -> List(Y, FU_BRU,  BR_LEZ,     OP1_RS,   OP2_X,   OPD_X),
     BGTZ    -> List(Y, FU_BRU,  BR_GTZ,     OP1_RS,   OP2_X,   OPD_X),
     BLTZ    -> List(Y, FU_BRU,  BR_LTZ,     OP1_RS,   OP2_X,   OPD_X),
     J       -> List(Y, FU_BRU,  BR_J,       OP1_RS,   OP2_X,   OPD_X),
     JAL     -> List(Y, FU_BRU,  BR_JAL,     OP1_RS,   OP2_X,   OPD_31),
     JR      -> List(Y, FU_BRU,  BR_JR,      OP1_RS,   OP2_X,   OPD_X),
     JALR    -> List(Y, FU_BRU,  BR_JALR,    OP1_RS,   OP2_X,   OPD_RD),

     // MDU instructions
     MFHI    -> List(Y, FU_MDU,  MDU_MFHI,   OP1_X,    OP2_X,   OPD_RD),
     MFLO    -> List(Y, FU_MDU,  MDU_MFLO,   OP1_X,    OP2_X,   OPD_RD),
     MUL     -> List(Y, FU_MDU,  MDU_MUL,    OP1_RS,   OP2_RT,  OPD_RD),
     MULT    -> List(Y, FU_MDU,  MDU_MULT,   OP1_RS,   OP2_RT,  OPD_HL),
     MULTU   -> List(Y, FU_MDU,  MDU_MULTU,  OP1_RS,   OP2_RT,  OPD_HL),
     DIV     -> List(Y, FU_MDU,  MDU_DIV,    OP1_RS,   OP2_RT,  OPD_HL),
     DIVU    -> List(Y, FU_MDU,  MDU_DIVU,   OP1_RS,   OP2_RT,  OPD_HL),

     // LSU instructions
     LW      -> List(Y, FU_LSU,  LSU_LW,     OP1_RSO,  OP2_X,   OPD_RT),
     LH      -> List(Y, FU_LSU,  LSU_LH,     OP1_RSO,  OP2_X,   OPD_RT),
     LHU     -> List(Y, FU_LSU,  LSU_LHU,    OP1_RSO,  OP2_X,   OPD_RT),
     LB      -> List(Y, FU_LSU,  LSU_LB,     OP1_RSO,  OP2_X,   OPD_RT),
     LBU     -> List(Y, FU_LSU,  LSU_LBU,    OP1_RSO,  OP2_X,   OPD_RT),
     LWL     -> List(Y, FU_LSU,  LSU_LWL,    OP1_RSO,  OP2_RT,  OPD_RT),
     LWR     -> List(Y, FU_LSU,  LSU_LWR,    OP1_RSO,  OP2_RT,  OPD_RT),
     SW      -> List(Y, FU_LSU,  LSU_SW,     OP1_RSO,  OP2_RT,  OPD_X),
     SH      -> List(Y, FU_LSU,  LSU_SH,     OP1_RSO,  OP2_RT,  OPD_X),
     SB      -> List(Y, FU_LSU,  LSU_SB,     OP1_RSO,  OP2_RT,  OPD_X),
     SWL     -> List(Y, FU_LSU,  LSU_SWL,    OP1_RSO,  OP2_RT,  OPD_X),
     SWR     -> List(Y, FU_LSU,  LSU_SWR,    OP1_RSO,  OP2_RT,  OPD_X),
  ))

  val (valid: Bool) :: fu_type :: fu_op :: op1_sel :: op2_sel :: rd_sel :: Nil = csignals
  assert (valid)

  val instr = fu_in.instr.asTypeOf(new Instr)
  val oprd_idx = Mux1H(Array(
    (rd_sel === OPD_RD) -> instr.rd_idx,
    (rd_sel === OPD_RT) -> instr.rt_idx,
    (rd_sel === OPD_31) -> 31.U,
  ))

  /* fu_out IO */
  io.fu_out.bits.fu_type := fu_type
  io.fu_out.bits.fu_op := fu_op
  io.fu_out.bits.op1_sel := op1_sel
  io.fu_out.bits.op2_sel := op2_sel
  io.fu_out.bits.rd_sel := rd_sel
  io.fu_out.bits.ex := 0.U.asTypeOf(new CP0Exception)

  /* register RW */
  val (instr_id, c) = Counter(io.fu_out.fire(), 1 << conf.INSTR_ID_SZ)
  io.rfio.rs_idx := instr.rs_idx
  io.rfio.rt_idx := instr.rt_idx
  io.rfio.wen := io.fu_out.fire() && rd_sel =/= OPD_X
  io.rfio.wid := instr_id
  io.rfio.rd_idx := oprd_idx

  /* branch check */
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val Ia = (fu_in.pc + 4.U + (se_imm << 2))(31, 0)
  val Ja = Cat(Seq(fu_in.pc(31, 28), instr.addr, 0.U(2.W)))
  val JRa = io.rfio.rs_data.bits
  /* br_info={34:ready, 33:jump, 32:wb, 31..0:target} */
  val br_info = Mux1H(Array(
    (fu_op === BR_EQ)   -> Cat(io.rfio.rs_data.valid && io.rfio.rt_data.valid, io.rfio.rs_data.bits === io.rfio.rt_data.bits, N, Ia),
    (fu_op === BR_NE)   -> Cat(io.rfio.rs_data.valid && io.rfio.rt_data.valid, io.rfio.rs_data.bits =/= io.rfio.rt_data.bits, N, Ia),
    (fu_op === BR_LEZ)  -> Cat(io.rfio.rs_data.valid, io.rfio.rs_data.bits.asSInt <= 0.S, N, Ia),
    (fu_op === BR_GTZ)  -> Cat(io.rfio.rs_data.valid, io.rfio.rs_data.bits.asSInt > 0.S, N, Ia),
    (fu_op === BR_LTZ)  -> Cat(io.rfio.rs_data.valid, io.rfio.rs_data.bits.asSInt < 0.S, N, Ia),
    (fu_op === BR_J)    -> Cat(Y, Y, N, Ja),
    (fu_op === BR_JAL)  -> Cat(Y, Y, Y, Ja),
    (fu_op === BR_JR)   -> Cat(Y, Y, N, JRa),
    (fu_op === BR_JALR) -> Cat(Y, Y, Y, JRa)))
  val br_ready = fu_type =/= FU_BRU || br_info(34)

  io.fu_in.ready := (io.fu_out.ready && br_ready) || !fu_valid
  io.br_flush.valid := io.fu_out.fire() && fu_type === FU_BRU && br_info(34) && br_info(33)
  io.br_flush.bits.br_target := br_info(31, 0)

  /* wb */
  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb.v := fu_type === FU_BRU && br_info(32)
  io.fu_out.bits.wb.id := instr_id
  io.fu_out.bits.wb.pc := fu_in.pc
  io.fu_out.bits.wb.instr := instr
  /* only valid for bru */
  io.fu_out.bits.wb.rd_idx := oprd_idx
  io.fu_out.bits.wb.wen := br_info(32)
  io.fu_out.bits.wb.data := fu_in.pc + 8.U
  /* only valid for bru */

  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_BRIDU) {
    printf("%d: BRIDU: fu_in={pc:%x, instr:%x}, fu_valid:%b, rd_idx=%d, se_imm=%x, Ia=%x, Ja=%x, JRa=%x, br_info=%x, br_ready=%b\n", GTimer(), fu_in.pc, fu_in.instr.asUInt, fu_valid, oprd_idx, se_imm, Ia, Ja, JRa, br_info, br_ready);
    io.fu_in.dump("BRIDU.io.fu_in")
    io.fu_out.dump("BRIDU.io.fu_out")
    io.rfio.dump("BRIDU.io.rfio")
    io.br_flush.dump("BRIDU.io.br_flush")
    io.ex_flush.dump("BRIDU.io.ex_flush")
  }
}

