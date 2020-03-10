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
    val can_log_now = Input(Bool())
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)

  // instruction decode stage
  val csignals = ListLookup(fu_in.instr,
    List(N, FU_X, FU_OP_X, OP1_X, OP2_X, OPD_X), Array(
      /* instr | fu_type  |  fu_op  |  op1_sel  |  op2_sel |  opd_sel */
     // ALU instructions
     LUI     -> List(Y, FU_ALU,  ALU_LUI,    OP1_X,    OP2_IMU, OPD_RT),
     ADD     -> List(Y, FU_ALU,  ALU_ADD_OV, OP1_RS,   OP2_RT,  OPD_RD),
     ADDU    -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_RT,  OPD_RD),
     SUB     -> List(Y, FU_ALU,  ALU_SUB_OV, OP1_RS,   OP2_RT,  OPD_RD),
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
     ADDI    -> List(Y, FU_ALU,  ALU_ADD_OV, OP1_RS,   OP2_IMI, OPD_RT),
     ADDIU   -> List(Y, FU_ALU,  ALU_ADD,    OP1_RS,   OP2_IMI, OPD_RT),
     ANDI    -> List(Y, FU_ALU,  ALU_AND,    OP1_RS,   OP2_IMZ, OPD_RT),
     ORI     -> List(Y, FU_ALU,  ALU_OR,     OP1_RS,   OP2_IMZ, OPD_RT),
     XORI    -> List(Y, FU_ALU,  ALU_XOR,    OP1_RS,   OP2_IMZ, OPD_RT),
     MOVN    -> List(Y, FU_ALU,  ALU_MOVN,   OP1_RS,   OP2_RT,  OPD_RD),
     MOVZ    -> List(Y, FU_ALU,  ALU_MOVZ,   OP1_RS,   OP2_RT,  OPD_RD),
     CLZ     -> List(Y, FU_ALU,  ALU_CLZ,    OP1_RS,   OP2_X,   OPD_RD),

     // BRU instructions
     BEQ     -> List(Y, FU_BRU,  BR_EQ,      OP1_RS,   OP2_RT,  OPD_X),
     BNE     -> List(Y, FU_BRU,  BR_NE,      OP1_RS,   OP2_RT,  OPD_X),
     BLEZ    -> List(Y, FU_BRU,  BR_LEZ,     OP1_RS,   OP2_X,   OPD_X),
     BGEZ    -> List(Y, FU_BRU,  BR_GEZ,     OP1_RS,   OP2_X,   OPD_X),
     BLTZ    -> List(Y, FU_BRU,  BR_LTZ,     OP1_RS,   OP2_X,   OPD_X),
     BGTZ    -> List(Y, FU_BRU,  BR_GTZ,     OP1_RS,   OP2_X,   OPD_X),
     BGEZAL  -> List(Y, FU_BRU,  BR_GEZAL,   OP1_RS,   OP2_X,   OPD_31),
     BLTZAL  -> List(Y, FU_BRU,  BR_LTZAL,   OP1_RS,   OP2_X,   OPD_31),
     J       -> List(Y, FU_BRU,  BR_J,       OP1_RS,   OP2_X,   OPD_X),
     JAL     -> List(Y, FU_BRU,  BR_JAL,     OP1_RS,   OP2_X,   OPD_31),
     JR      -> List(Y, FU_BRU,  BR_JR,      OP1_RS,   OP2_X,   OPD_X),
     JALR    -> List(Y, FU_BRU,  BR_JALR,    OP1_RS,   OP2_X,   OPD_RD),

     // MDU instructions
     MFHI    -> List(Y, FU_MDU,  MDU_MFHI,   OP1_X,    OP2_X,   OPD_RD),
     MFLO    -> List(Y, FU_MDU,  MDU_MFLO,   OP1_X,    OP2_X,   OPD_RD),
     MTHI    -> List(Y, FU_MDU,  MDU_MTHI,   OP1_RS,   OP2_X,   OPD_X),
     MTLO    -> List(Y, FU_MDU,  MDU_MTLO,   OP1_RS,   OP2_X,   OPD_X),
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

     // PRU instructions
     SYSCALL -> List(Y, FU_PRU,  PRU_SYSCALL,OP1_X,    OP2_X,   OPD_X),
     BREAK   -> List(Y, FU_PRU,  PRU_BREAK,  OP1_X,    OP2_X,   OPD_X),
     ERET    -> List(Y, FU_PRU,  PRU_ERET,   OP1_X,    OP2_X,   OPD_X),
     MFC0    -> List(Y, FU_PRU,  PRU_MFC0,   OP1_X,    OP2_X,   OPD_RT),
     MTC0    -> List(Y, FU_PRU,  PRU_MTC0,   OP1_RT,   OP2_X,   OPD_X),
     CACHE   -> List(Y, FU_PRU,  PRU_CACHE,  OP1_X,    OP2_X,   OPD_X),
     SYNC    -> List(Y, FU_PRU,  PRU_SYNC,   OP1_X,    OP2_X,   OPD_X),
     PREF    -> List(Y, FU_PRU,  PRU_PREF,   OP1_X,    OP2_X,   OPD_X),
     TLBP    -> List(Y, FU_PRU,  PRU_TLBP,   OP1_X,    OP2_X,   OPD_X),
     TLBR    -> List(Y, FU_PRU,  PRU_TLBR,   OP1_X,    OP2_X,   OPD_X),
     TLBWI   -> List(Y, FU_PRU,  PRU_TLBWI,  OP1_X,    OP2_X,   OPD_X),
     TLBWR   -> List(Y, FU_PRU,  PRU_TLBWR,  OP1_X,    OP2_X,   OPD_X),
  ))

  val (valid: Bool) :: fu_type_ :: fu_op_ :: op1_sel_ :: op2_sel_ :: opd_sel_ :: Nil = csignals

  val has_ex = !valid || fu_in.ex.et =/= ET_None
  val fu_type = Mux(has_ex, FU_PRU,   fu_type_)
  val fu_op   = Mux(has_ex, PRU_OP_X, fu_op_)
  val op1_sel = Mux(has_ex, OP1_X,    op1_sel_)
  val op2_sel = Mux(has_ex, OP2_X,    op2_sel_)
  val opd_sel = Mux(has_ex, OPD_X,    opd_sel_)

  // assert (valid, "%d: invalid instruction at %x", GTimer(), fu_in.pc)

  val instr = fu_in.instr.asTypeOf(new Instr)
  val oprd_idx = Mux1H(Array(
    (opd_sel === OPD_RD) -> instr.rd_idx,
    (opd_sel === OPD_RT) -> instr.rt_idx,
    (opd_sel === OPD_31) -> 31.U,
  ))

  /* fu_out IO */
  io.fu_out.bits.fu_type := fu_type
  io.fu_out.bits.fu_op := fu_op
  io.fu_out.bits.op1_sel := op1_sel
  io.fu_out.bits.op2_sel := op2_sel
  io.fu_out.bits.opd_sel := opd_sel
  io.fu_out.bits.ex.et := Mux(valid, fu_in.ex.et, ET_RI)
  io.fu_out.bits.ex.code := Mux(valid, fu_in.ex.code, EC_RI)
  io.fu_out.bits.ex.addr := fu_in.ex.addr
  io.fu_out.bits.ex.asid := 0.U

  /* register RW */
  val (instr_id, c) = Counter(io.fu_out.fire(), 1 << conf.INSTR_ID_SZ)
  io.rfio.rs_idx := instr.rs_idx
  io.rfio.rt_idx := instr.rt_idx
  io.rfio.wen := io.fu_out.fire() && opd_sel =/= OPD_X
  io.rfio.wid := instr_id
  io.rfio.rd_idx := oprd_idx

  /* branch check */
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val Ia = (fu_in.pc + 4.U + (se_imm << 2))(31, 0)
  val Ja = Cat(Seq(fu_in.pc(31, 28), instr.addr, 0.U(2.W)))
  val Za = fu_in.pc + (instr.imm << 2).asTypeOf(SInt(32.W)).asUInt + 4.U
  val JRa = io.rfio.rs_data.bits
  val rs_ready = io.rfio.rs_data.valid
  val rt_ready = io.rfio.rt_data.valid
  val rs_data = io.rfio.rs_data.bits
  val rt_data = io.rfio.rt_data.bits
  /* br_info={34:ready, 33:jump, 32:wb, 31..0:target} */
  val br_info = Mux1H(Array(
    (fu_op === BR_EQ)   -> Cat(rs_ready && rt_ready, rs_data === rt_data, N, Ia),
    (fu_op === BR_NE)   -> Cat(rs_ready && rt_ready, rs_data =/= rt_data, N, Ia),
    (fu_op === BR_LEZ)  -> Cat(rs_ready, rs_data.asSInt <= 0.S, N, Ia),
    (fu_op === BR_GEZ)  -> Cat(rs_ready, rs_data.asSInt >= 0.S, N, Ia),
    (fu_op === BR_LTZ)  -> Cat(rs_ready, rs_data.asSInt < 0.S, N, Ia),
    (fu_op === BR_GTZ)  -> Cat(rs_ready, rs_data.asSInt > 0.S, N, Ia),
    (fu_op === BR_GEZAL)-> Cat(rs_ready, rs_data.asSInt >= 0.S, Y, Za),
    (fu_op === BR_LTZAL)-> Cat(rs_ready, rs_data.asSInt < 0.S, Y, Za),
    (fu_op === BR_J)    -> Cat(Y, Y, N, Ja),
    (fu_op === BR_JAL)  -> Cat(Y, Y, Y, Ja),
    (fu_op === BR_JR)   -> Cat(Y, Y, N, JRa),
    (fu_op === BR_JALR) -> Cat(Y, Y, Y, JRa)))
  val br_ready = fu_type =/= FU_BRU || br_info(34)

  io.fu_in.ready := (io.fu_out.ready && br_ready) || !fu_valid
  io.br_flush.valid := io.fu_out.fire() && fu_type === FU_BRU && br_info(34) && br_info(33)
  io.br_flush.bits.br_target := br_info(31, 0)

  /* wb */
  val is_delayslot = RegInit(N)
  when (io.fu_out.fire() && fu_type === FU_BRU) { is_delayslot := Y }
  .elsewhen (io.fu_out.fire()) { is_delayslot := N }
  io.fu_out.valid := fu_valid && !io.ex_flush.valid
  io.fu_out.bits.wb.v := fu_type === FU_BRU && br_info(32)
  io.fu_out.bits.wb.id := instr_id
  io.fu_out.bits.wb.pc := fu_in.pc
  io.fu_out.bits.wb.instr := instr
  /* only valid for bru */
  io.fu_out.bits.wb.rd_idx := oprd_idx
  io.fu_out.bits.wb.wen := io.fu_out.bits.wb.v
  io.fu_out.bits.wb.data := fu_in.pc + 8.U
  io.fu_out.bits.wb.is_ds := is_delayslot
  io.fu_out.bits.wb.is_br := fu_type === FU_BRU
  io.fu_out.bits.wb.npc := Mux(io.br_flush.valid,
    io.br_flush.bits.br_target, fu_in.pc + 4.U)
  io.fu_out.bits.wb.ip7 := N
  /* only valid for bru */

  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_BRIDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("%d: BRIDU: fu_in={pc:%x, instr:%x}, fu_valid:%b, rd_idx=%d, se_imm=%x, Ia=%x, Ja=%x, JRa=%x, br_info=%x, br_ready=%b, is_ds=%b\n", GTimer(), fu_in.pc, fu_in.instr.asUInt, fu_valid, oprd_idx, se_imm, Ia, Ja, JRa, br_info, br_ready, is_delayslot);
    io.fu_in.dump("BRIDU.io.fu_in")
    io.fu_out.dump("BRIDU.io.fu_out")
    io.rfio.dump("BRIDU.io.rfio")
    io.br_flush.dump("BRIDU.io.br_flush")
    io.ex_flush.dump("BRIDU.io.ex_flush")
  }
}

