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
    val fu_in = Flipped(DecoupledIO(new IFU_BRIDU_IO))
    val fu_out = DecoupledIO(new BRIDU_PRALU_IO)
    val rfreq = new RegFileReq
    val br_flush = ValidIO(new FlushIO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
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
     JAL     -> List(Y, FU_BRU,  BR_JAL,     OP1_RS,   OP2_X,   DEST_31),
     JR      -> List(Y, FU_BRU,  BR_JR,      OP1_RS,   OP2_X,   DEST_RD),
     JALR    -> List(Y, FU_BRU,  BR_JALR,    OP1_RS,   OP2_X,   DEST_RD),

     // LSU instructions
     // LW      -> List(Y, FU_LSU,  LSU_LW,     OP1_)
  ))

  val (valid: Bool) :: fu_type :: fu_op :: op1_sel :: op2_sel :: rd_sel :: Nil = csignals
  assert (valid)

  val instr = fu_in.instr.asTypeOf(new Instr)
  val dest_ridx = Mux1H(Array(
    (rd_sel === DEST_RD) -> instr.rd_idx,
    (rd_sel === DEST_RT) -> instr.rt_idx,
    (rd_sel === DEST_31) -> 31.U,
  ))

  /* fu_out IO */
  io.fu_out.bits.fu_type := fu_type
  io.fu_out.bits.fu_op := fu_op
  io.fu_out.bits.op1_sel := op1_sel
  io.fu_out.bits.op2_sel := op2_sel
  io.fu_out.bits.rd_sel := rd_sel
  io.fu_out.bits.ex := 0.U.asTypeOf(new CP0Exception)

  /* register RW */
  io.rfreq.rs_idx := instr.rs_idx
  io.rfreq.rt_idx := instr.rt_idx
  io.rfreq.dest_ridx.valid := fu_valid
  io.rfreq.dest_ridx.bits := dest_ridx

  /* branch check */
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val Ia = (fu_in.pc + (se_imm << 2))(31, 0)
  val Ja = Cat(Seq(fu_in.pc(31, 28), instr.addr, 0.U(2.W)))
  val JRa = io.rfreq.rs_data.bits
  /* br_info={34:ready, 33:jump, 32:wb, 31..0:target} */
  val br_info = Mux1H(Array(
    (fu_op === BR_EQ) -> Cat(io.rfreq.rs_data.valid && io.rfreq.rt_data.valid, io.rfreq.rs_data.bits === io.rfreq.rt_data.bits, N, Ia),
    (fu_op === BR_NE) -> Cat(io.rfreq.rs_data.valid && io.rfreq.rt_data.valid, io.rfreq.rs_data.bits =/= io.rfreq.rt_data.bits, N, Ia),
    (fu_op === BR_LEZ) -> Cat(io.rfreq.rs_data.valid, io.rfreq.rs_data.bits.asSInt <= 0.S, N, Ia),
    (fu_op === BR_GTZ) -> Cat(io.rfreq.rs_data.valid, io.rfreq.rs_data.bits.asSInt > 0.S, N, Ia),
    (fu_op === BR_LTZ) -> Cat(io.rfreq.rs_data.valid, io.rfreq.rs_data.bits.asSInt < 0.S, N, Ia),
    (fu_op === BR_J) -> Cat(Y, Y, N, Ja),
    (fu_op === BR_JAL) -> Cat(Y, Y, Y, Ja),
    (fu_op === BR_JR) -> Cat(Y, Y, N, JRa),
    (fu_op === BR_JALR) -> Cat(Y, Y, Y, JRa)))

  io.fu_in.ready := (io.fu_out.ready && br_info(34)) || !fu_valid
  io.br_flush.valid := fu_valid && fu_type === FU_BRU && br_info(34) && br_info(33)
  io.br_flush.bits.br_target := br_info(31, 0)

  /* wb */
  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb.pc := fu_in.pc
  io.fu_out.bits.wb.instr := instr
  /* only valid for bru */
  io.fu_out.bits.wb.rd_idx := dest_ridx
  io.fu_out.bits.wb.wen := br_info(32)
  io.fu_out.bits.wb.data := fu_in.pc + 8.U
  /* only valid for bru */

  when (!io.fu_in.fire() && (io.br_flush.valid || io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_BRIDU) {
    printf("%d: BRIDU: fu_in={pc:%x, instr:%x}, fu_valid:%b, rd_idx=%d, se_imm=%x, Ia=%x, Ja=%x, JRa=%x, br_info=%x\n", GTimer(), fu_in.pc, fu_in.instr.asUInt, fu_valid, dest_ridx, se_imm, Ia, Ja, JRa, br_info);
    io.fu_in.dump("BRIDU.io.fu_in")
    io.fu_out.dump("BRIDU.io.fu_out")
    io.rfreq.dump("BRIDU.io.rfreq")
    io.br_flush.dump("BRIDU.io.br_flush")
    io.ex_flush.dump("BRIDU.io.ex_flush")
  }
}

