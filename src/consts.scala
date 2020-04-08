package woop

import chisel3._
import chisel3.util._
import woop.configs._

trait MemConsts {
  // MX
  val MX_SZ = 1
  val MX_X  = 0.U(MX_SZ.W)
  val MX_RD = 0.U(MX_SZ.W)
  val MX_WR = 1.U(MX_SZ.W)

  val ML_SZ = 2
  val ML_X  = 0.U(ML_SZ.W)
  val ML_1  = 0.U(ML_SZ.W)
  val ML_2  = 1.U(ML_SZ.W)
  val ML_3  = 2.U(ML_SZ.W)
  val ML_4  = 3.U(ML_SZ.W)
}

trait CP0Consts {
  // exception type
  val ET_WIDTH = 5
  val ET_None       =   0.U(ET_WIDTH.W)
  val ET_EJTAG      =   1.U(ET_WIDTH.W)
  val ET_RESET      =   2.U(ET_WIDTH.W)
  val ET_SOFT_RST   =   3.U(ET_WIDTH.W)
  val ET_NMI        =   4.U(ET_WIDTH.W)
  val ET_MCheck     =   5.U(ET_WIDTH.W)
  val ET_ADDR_ERR   =   6.U(ET_WIDTH.W)
  val ET_TLB_REFILL =   7.U(ET_WIDTH.W)
  val ET_TLB_Inv    =   8.U(ET_WIDTH.W)
  val ET_TLB_Mod    =   9.U(ET_WIDTH.W)
  val ET_CACHE_ERR  =  10.U(ET_WIDTH.W)
  val ET_BUS_ERR    =  11.U(ET_WIDTH.W)
  val ET_Ov         =  12.U(ET_WIDTH.W)
  val ET_Tr         =  13.U(ET_WIDTH.W)
  val ET_Sys        =  14.U(ET_WIDTH.W)
  val ET_Bp         =  15.U(ET_WIDTH.W)
  val ET_RI         =  16.U(ET_WIDTH.W)
  val ET_CpU        =  17.U(ET_WIDTH.W)
  val ET_FPE        =  18.U(ET_WIDTH.W)
  val ET_C3E        =  18.U(ET_WIDTH.W)
  val ET_WATCH      =  20.U(ET_WIDTH.W)
  val ET_Int        =  21.U(ET_WIDTH.W)
  val ET_Eret       =  22.U(ET_WIDTH.W)

  val EC_WIDTH = 5
  val EC_Int  =  0.U(EC_WIDTH.W)  // - Interrupt
  val EC_Mod  =  1.U(EC_WIDTH.W)  // * TLB modification
  val EC_TLBL =  2.U(EC_WIDTH.W)  // * TLB load
  val EC_TLBS =  3.U(EC_WIDTH.W)  // * TLB store
  val EC_AdEL =  4.U(EC_WIDTH.W)  // * Address Load
  val EC_AdES =  5.U(EC_WIDTH.W)  // * Address Store
  val EC_IBE  =  6.U(EC_WIDTH.W)  //   Bus error(IF)
  val EC_DBE  =  7.U(EC_WIDTH.W)  //   Bus error(data)
  val EC_Sys  =  8.U(EC_WIDTH.W)  // * Syscall
  val EC_Bp   =  9.U(EC_WIDTH.W)  //   Break Point
  val EC_RI   = 10.U(EC_WIDTH.W)  // * Reserved instruction
  val EC_CpU  = 11.U(EC_WIDTH.W)  // * Coprocessor unusable
  val EC_Ov   = 12.U(EC_WIDTH.W)  // * Arithmetic overflow
  val EC_Tr   = 13.U(EC_WIDTH.W)  // * Trap
}

trait InstrConsts {
  val REG_SZ    = 5;
}

trait InstrPattern {
  val LUI   = BitPat("b00111100000?????????????????????")
  val ADD   = BitPat("b000000???????????????00000100000")
  val ADDU  = BitPat("b000000???????????????00000100001")
  val SUB   = BitPat("b000000???????????????00000100010")
  val SUBU  = BitPat("b000000???????????????00000100011")
  val SLT   = BitPat("b000000???????????????00000101010")
  val SLTU  = BitPat("b000000???????????????00000101011")
  val AND   = BitPat("b000000???????????????00000100100")
  val OR    = BitPat("b000000???????????????00000100101")
  val XOR   = BitPat("b000000???????????????00000100110")
  val NOR   = BitPat("b000000???????????????00000100111")
  val SLTI  = BitPat("b001010??????????????????????????")
  val SLTIU = BitPat("b001011??????????????????????????")
  val SRA   = BitPat("b00000000000???????????????000011")
  val SRL   = BitPat("b00000000000???????????????000010")
  val SLL   = BitPat("b00000000000???????????????000000")
  val SRAV  = BitPat("b000000???????????????00000000111")
  val SRLV  = BitPat("b000000???????????????00000000110")
  val SLLV  = BitPat("b000000???????????????00000000100")

  val ADDI  = BitPat("b001000??????????????????????????")
  val ADDIU = BitPat("b001001??????????????????????????")
  val ANDI  = BitPat("b001100??????????????????????????")
  val ORI   = BitPat("b001101??????????????????????????")
  val XORI  = BitPat("b001110??????????????????????????")
  val MOVN  = BitPat("b000000???????????????00000001011")
  val MOVZ  = BitPat("b000000???????????????00000001010")
  def CLZ   = BitPat("b011100???????????????00000100000")

  def BEQ    = BitPat("b000100??????????????????????????")
  def BNE    = BitPat("b000101??????????????????????????")
  def BLEZ   = BitPat("b000110?????00000????????????????")
  def BGTZ   = BitPat("b000111?????00000????????????????")
  def BLTZ   = BitPat("b000001?????00000????????????????")
  def BGEZ   = BitPat("b000001?????00001????????????????")
  def BGEZAL = BitPat("b000001?????10001????????????????")
  def BLTZAL = BitPat("b000001?????10000????????????????")
  def J      = BitPat("b000010??????????????????????????")
  def JAL    = BitPat("b000011??????????????????????????")
  def JR     = BitPat("b000000?????0000000000?????001000")
  def JALR   = BitPat("b000000???????????????00000001001")
  def BEQL   = BitPat("b010100??????????????????????????")
  def BNEL   = BitPat("b010101??????????????????????????")
  def BLEZL  = BitPat("b010110?????00000????????????????")
  def BGTZL  = BitPat("b010111?????00000????????????????")
  def BLTZL  = BitPat("b000001?????00010????????????????")
  def BGEZL  = BitPat("b000001?????00011????????????????")
  def BGEZALL= BitPat("b000001?????10011????????????????")
  def BLTZALL= BitPat("b000001?????10010????????????????")

  val LW    = BitPat("b100011??????????????????????????")
  val LH    = BitPat("b100001??????????????????????????")
  val LHU   = BitPat("b100101??????????????????????????")
  val LB    = BitPat("b100000??????????????????????????")
  val LBU   = BitPat("b100100??????????????????????????")
  val LWL   = BitPat("b100010??????????????????????????")
  val LWR   = BitPat("b100110??????????????????????????")
  val SW    = BitPat("b101011??????????????????????????")
  val SH    = BitPat("b101001??????????????????????????")
  val SB    = BitPat("b101000??????????????????????????")
  val SWL   = BitPat("b101010??????????????????????????")
  val SWR   = BitPat("b101110??????????????????????????")
  val LL    = BitPat("b110000??????????????????????????")
  val SC    = BitPat("b111000??????????????????????????")

  val MFHI  = BitPat("b0000000000000000?????00000010000")
  val MFLO  = BitPat("b0000000000000000?????00000010010")
  def MTHI  = BitPat("b000000?????000000000000000010001")
  def MTLO  = BitPat("b000000?????000000000000000010011")
  val MUL   = BitPat("b011100???????????????00000000010")
  val MULT  = BitPat("b000000??????????0000000000011000")
  val MULTU = BitPat("b000000??????????0000000000011001")
  val DIV   = BitPat("b000000??????????0000000000011010")
  val DIVU  = BitPat("b000000??????????0000000000011011")
  val MADD  = BitPat("b011100??????????0000000000000000")
  val MADDU = BitPat("b011100??????????0000000000000001")
  val MSUB  = BitPat("b011100??????????0000000000000100")
  val MSUBU = BitPat("b011100??????????0000000000000101")

  def SYSCALL= BitPat("b000000????????????????????001100")
  def ERET   = BitPat("b01000010000000000000000000011000")
  def MFC0   = BitPat("b01000000000??????????00000000???")
  def MTC0   = BitPat("b01000000100??????????00000000???")
  def TLBR   = BitPat("b01000010000000000000000000000001")
  def TLBWI  = BitPat("b01000010000000000000000000000010")
  def TLBWR  = BitPat("b01000010000000000000000000000110")
  def TLBP   = BitPat("b01000010000000000000000000001000")
  def BREAK  = BitPat("b000000????????????????????001101")
  def CACHE  = BitPat("b101111??????????????????????????")
  def PREF   = BitPat("b110011??????????????????????????")
  def SYNC   = BitPat("b000000000000000000000?????001111")
  def TGE    = BitPat("b000000????????????????????110000")
  def TGEU   = BitPat("b000000????????????????????110001")
  def TLT    = BitPat("b000000????????????????????110010")
  def TLTU   = BitPat("b000000????????????????????110011")
  def TEQ    = BitPat("b000000????????????????????110100")
  def TNE    = BitPat("b000000????????????????????110110")
  def TGEI   = BitPat("b000001?????01000????????????????")
  def TGEIU  = BitPat("b000001?????01001????????????????")
  def TLTI   = BitPat("b000001?????01010????????????????")
  def TLTIU  = BitPat("b000001?????01011????????????????")
  def TEQI   = BitPat("b000001?????01100????????????????")
  def TNEI   = BitPat("b000001?????01110????????????????")
}


trait ISUConsts
{
  val Y      = true.B
  val N      = false.B

  val FU_OP_SZ = 5
  val FU_OP_X = 0.U(FU_OP_SZ.W)

  // Functional Unit Select Signal
  val FU_TYPE_SZ = 3
  val FU_X   = 0.U(FU_TYPE_SZ.W)
  val FU_ALU = 1.U(FU_TYPE_SZ.W)
  val FU_BRU = 2.U(FU_TYPE_SZ.W)
  val FU_LSU = 3.U(FU_TYPE_SZ.W)
  val FU_MDU = 4.U(FU_TYPE_SZ.W)
  val FU_PRU = 5.U(FU_TYPE_SZ.W)

  // RS Operand Select Signal
  val OP1_SEL_SZ = 2
  val OP1_X    = 0.U(OP1_SEL_SZ.W)
  val OP1_RS   = 1.U(OP1_SEL_SZ.W) // Register Source #1
  val OP1_RT   = 2.U(OP1_SEL_SZ.W)
  val OP1_RSO  = 3.U(OP1_SEL_SZ.W) // offset(rs)

  // RT Operand Select Signal
  val OP2_SEL_SZ = 3
  val OP2_X   = 0.U(OP2_SEL_SZ.W)
  val OP2_RS  = 1.U(OP2_SEL_SZ.W) // Register Source #2
  val OP2_RT  = 2.U(OP2_SEL_SZ.W) // Register Source #2
  val OP2_IMI = 3.U(OP2_SEL_SZ.W) // immediate, I-type
  val OP2_IMU = 4.U(OP2_SEL_SZ.W) // immediate, U-type
  val OP2_IMZ = 5.U(OP2_SEL_SZ.W) // zero-extended immediate, I-type
  val OP2_SA  = 6.U(OP2_SEL_SZ.W) // shift amount

  // REG Dest Select Signal
  val OPD_SEL_SZ = 2
  val OPD_X  = 0.U(OPD_SEL_SZ.W)
  val OPD_HL = 0.U(OPD_SEL_SZ.W)
  val OPD_RD = 1.U(OPD_SEL_SZ.W)
  val OPD_RT = 2.U(OPD_SEL_SZ.W)
  val OPD_31 = 3.U(OPD_SEL_SZ.W)
}

trait PRUConsts extends ISUConsts {
  val PRU_OP_X    =  0.U(FU_OP_SZ.W)
  val PRU_SYSCALL =  1.U(FU_OP_SZ.W)
  val PRU_ERET    =  2.U(FU_OP_SZ.W)
  val PRU_MFC0    =  3.U(FU_OP_SZ.W)
  val PRU_MTC0    =  4.U(FU_OP_SZ.W)
  val PRU_TLBR    =  5.U(FU_OP_SZ.W)
  val PRU_TLBWI   =  6.U(FU_OP_SZ.W)
  val PRU_TLBWR   =  7.U(FU_OP_SZ.W)
  val PRU_TLBP    =  8.U(FU_OP_SZ.W)
  val PRU_BREAK   =  9.U(FU_OP_SZ.W)
  val PRU_CACHE   = 10.U(FU_OP_SZ.W)
  val PRU_PREF    = 11.U(FU_OP_SZ.W)
  val PRU_SYNC    = 12.U(FU_OP_SZ.W)
  val PRU_TGE     = 13.U(FU_OP_SZ.W)
  val PRU_TGEU    = 14.U(FU_OP_SZ.W)
  val PRU_TLT     = 15.U(FU_OP_SZ.W)
  val PRU_TLTU    = 16.U(FU_OP_SZ.W)
  val PRU_TEQ     = 17.U(FU_OP_SZ.W)
  val PRU_TNE     = 18.U(FU_OP_SZ.W)
  val PRU_TGEI    = 19.U(FU_OP_SZ.W)
  val PRU_TGEIU   = 20.U(FU_OP_SZ.W)
  val PRU_TLTI    = 21.U(FU_OP_SZ.W)
  val PRU_TLTIU   = 22.U(FU_OP_SZ.W)
  val PRU_TEQI    = 23.U(FU_OP_SZ.W)
  val PRU_TNEI    = 24.U(FU_OP_SZ.W)

  val CPR_INDEX     = "b00000_000".U(8.W)
  val CPR_RANDOM    = "b00001_000".U(8.W)
  val CPR_ENTRY_LO0 = "b00010_000".U(8.W)
  val CPR_ENTRY_LO1 = "b00011_000".U(8.W)
  val CPR_CONTEXT   = "b00100_000".U(8.W)
  val CPR_PAGEMASK  = "b00101_000".U(8.W)
  val CPR_WIRED     = "b00110_000".U(8.W)
  val CPR_BAD_VADDR = "b01000_000".U(8.W)
  val CPR_COUNT     = "b01001_000".U(8.W)
  val CPR_ENTRY_HI  = "b01010_000".U(8.W)
  val CPR_COMPARE   = "b01011_000".U(8.W)
  val CPR_STATUS    = "b01100_000".U(8.W)
  val CPR_CAUSE     = "b01101_000".U(8.W)
  val CPR_EPC       = "b01110_000".U(8.W)
  val CPR_PRID      = "b01111_000".U(8.W)
  val CPR_EBASE     = "b01111_001".U(8.W)
  val CPR_CONFIG    = "b10000_000".U(8.W)
  val CPR_CONFIG1   = "b10000_001".U(8.W)
}

// UInt definition cannot occur in Bundle subclass
trait BRUConsts extends ISUConsts {
  // Branch Operation Signal
  val BR_EQ    =  0.U(FU_OP_SZ.W)
  val BR_NE    =  1.U(FU_OP_SZ.W)
  val BR_LEZ   =  2.U(FU_OP_SZ.W)
  val BR_GEZ   =  3.U(FU_OP_SZ.W)
  val BR_LTZ   =  4.U(FU_OP_SZ.W)
  val BR_GTZ   =  5.U(FU_OP_SZ.W)
  val BR_GEZAL =  6.U(FU_OP_SZ.W)
  val BR_LTZAL =  7.U(FU_OP_SZ.W)
  val BR_J     =  8.U(FU_OP_SZ.W)  // Jump
  val BR_JAL   =  9.U(FU_OP_SZ.W)  // Jump Register
  val BR_JR    = 10.U(FU_OP_SZ.W)  // Jump Register
  val BR_JALR  = 11.U(FU_OP_SZ.W)  // Jump Register
}

trait ALUConsts extends ISUConsts {
  // ALU Operation Signal
  val ALU_ADD    =  0.U(FU_OP_SZ.W)
  val ALU_SUB    =  1.U(FU_OP_SZ.W)
  val ALU_SLL    =  2.U(FU_OP_SZ.W)
  val ALU_SRL    =  3.U(FU_OP_SZ.W)
  val ALU_SRA    =  4.U(FU_OP_SZ.W)
  val ALU_AND    =  5.U(FU_OP_SZ.W)
  val ALU_OR     =  6.U(FU_OP_SZ.W)
  val ALU_XOR    =  7.U(FU_OP_SZ.W)
  val ALU_NOR    =  8.U(FU_OP_SZ.W)
  val ALU_SLT    =  9.U(FU_OP_SZ.W)
  val ALU_SLTU   = 10.U(FU_OP_SZ.W)
  val ALU_LUI    = 11.U(FU_OP_SZ.W)
  val ALU_MOVN   = 12.U(FU_OP_SZ.W)
  val ALU_MOVZ   = 13.U(FU_OP_SZ.W)
  val ALU_ADD_OV = 14.U(FU_OP_SZ.W)
  val ALU_SUB_OV = 15.U(FU_OP_SZ.W)
  val ALU_CLZ    = 16.U(FU_OP_SZ.W)
}

/* contains temp node `Cat`, should be extended by Module */
trait LSUConsts extends ISUConsts with MemConsts {
  // LSU Operation Signal
  val LSU_E_SZ = 1
  val LSU_XE  = 0.U(LSU_E_SZ.W)
  val LSU_SE  = 0.U(LSU_E_SZ.W)
  val LSU_ZE  = 1.U(LSU_E_SZ.W)

  val LSU_L = 0.U(1.W)
  val LSU_R = 1.U(1.W)

  //                 1    1      2     1
  val LSU_OP_X = Cat(Y, MX_X,  ML_X, LSU_XE)
  val LSU_LW   = Cat(Y, MX_RD, ML_4, LSU_SE)
  val LSU_LB   = Cat(Y, MX_RD, ML_1, LSU_SE)
  val LSU_LBU  = Cat(Y, MX_RD, ML_1, LSU_ZE)
  val LSU_LH   = Cat(Y, MX_RD, ML_2, LSU_SE)
  val LSU_LHU  = Cat(Y, MX_RD, ML_2, LSU_ZE)
  val LSU_SW   = Cat(Y, MX_WR, ML_4, LSU_SE)
  val LSU_SB   = Cat(Y, MX_WR, ML_1, LSU_SE)
  val LSU_SH   = Cat(Y, MX_WR, ML_2, LSU_SE)
  //                 1    1      2     1
  val LSU_LWL  = Cat(N, MX_RD, ML_4, LSU_L)
  val LSU_LWR  = Cat(N, MX_RD, ML_4, LSU_R)
  val LSU_SWL  = Cat(N, MX_WR, ML_4, LSU_L)
  val LSU_SWR  = Cat(N, MX_WR, ML_4, LSU_R)
  //
  val LSU_LL   = Cat(Y, MX_RD, ML_4, LSU_ZE)
  val LSU_SC   = Cat(Y, MX_WR, ML_4, LSU_ZE)
}

/* contains temp node `Cat`, should be extended by Module */
trait MDUConsts extends ISUConsts {
  val WB_RD = 0.U(1.W)
  val WB_HL = 1.U(1.W)

  // MDU                     4          1
  val MDU_MFHI  = Cat("b0000".U(4.W), WB_RD)
  val MDU_MFLO  = Cat("b0001".U(4.W), WB_RD)
  val MDU_MTHI  = Cat("b0010".U(4.W), WB_HL)
  val MDU_MTLO  = Cat("b0011".U(4.W), WB_HL)
  val MDU_MUL   = Cat("b0100".U(4.W), WB_RD)
  val MDU_MULT  = Cat("b0101".U(4.W), WB_HL)
  val MDU_MULTU = Cat("b0110".U(4.W), WB_HL)
  val MDU_DIV   = Cat("b0111".U(4.W), WB_HL)
  val MDU_DIVU  = Cat("b1000".U(4.W), WB_HL)
  val MDU_MADD  = Cat("b1001".U(4.W), WB_HL)
  val MDU_MADDU = Cat("b1010".U(4.W), WB_HL)
  val MDU_MSUB  = Cat("b1011".U(4.W), WB_HL)
  val MDU_MSUBU = Cat("b1100".U(4.W), WB_HL)
}

trait CacheConsts {
  val CACHE_OP_SZ = 3

  val I_INDEX_INVALIDATE = "b000".U(CACHE_OP_SZ.W)
  val I_INDEX_LOAD_TAG   = "b001".U(CACHE_OP_SZ.W)
  val I_INDEX_STORE_TAG  = "b010".U(CACHE_OP_SZ.W)
  val I_HIT_INVALIDATE   = "b100".U(CACHE_OP_SZ.W)
  val I_FILL             = "b101".U(CACHE_OP_SZ.W)
  val I_FETCH_AND_LOCK   = "b111".U(CACHE_OP_SZ.W)

  val D_INDEX_WB_INV     = "b000".U(CACHE_OP_SZ.W)
  val D_INDEX_LOAD_TAG   = "b001".U(CACHE_OP_SZ.W)
  val D_INDEX_STORE_TAG  = "b010".U(CACHE_OP_SZ.W)
  val D_HIT_INVALIDATE   = "b100".U(CACHE_OP_SZ.W)
  val D_HIT_WB_INV       = "b101".U(CACHE_OP_SZ.W)
  val D_WB               = "b110".U(CACHE_OP_SZ.W)
  val D_FETCH_AND_LOCK   = "b111".U(CACHE_OP_SZ.W)

  val S_INDEX_WB_INV     = "b000".U(CACHE_OP_SZ.W)
  val S_INDEX_LOAD_TAG   = "b001".U(CACHE_OP_SZ.W)
  val S_INDEX_STORE_TAG  = "b010".U(CACHE_OP_SZ.W)
  val S_HIT_INVALIDATE   = "b100".U(CACHE_OP_SZ.W)
  val S_HIT_WB_INV       = "b101".U(CACHE_OP_SZ.W)
  val S_WB               = "b110".U(CACHE_OP_SZ.W)
  val S_FETCH_AND_LOCK   = "b111".U(CACHE_OP_SZ.W)
}

object consts extends InstrPattern
  with MemConsts
  with CP0Consts
  with ISUConsts
  with BRUConsts
  with PRUConsts
  // with LSUConsts
  // with MDUConsts
  with ALUConsts
  with InstrConsts
  with CacheConsts
{
}
