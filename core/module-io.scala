package woop
package core

import chisel3._
import chisel3.util._
import woop.configs._
import woop.consts._

class FlushIO extends Bundle {
  val br_target = Output(UInt(conf.DATA_WIDTH.W))
}

class BypassIO extends Bundle {
  val v = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val data = Output(UInt(conf.DATA_WIDTH.W))
}

class WriteBackIO extends Bundle {
  val v = Output(Bool())
  val id = Output(UInt(conf.INSTR_ID_SZ.W))
  val pc = Output(UInt(conf.DATA_WIDTH.W))
  val instr = Output(new Instr)
  val rd_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val data = Output(UInt(conf.DATA_WIDTH.W))
  val ip7 = Output(Bool())
  val is_ds = Output(Bool()) // is_delayslot
  val is_br = Output(Bool())
  val npc = Output(UInt(conf.DATA_WIDTH.W))
}

class EXU_OPS extends Bundle {
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(conf.DATA_WIDTH.W))
  val op2 = Output(UInt(conf.DATA_WIDTH.W))
}

class IFU_IDU_IO extends Bundle {
  val pc = Output(UInt(conf.DATA_WIDTH.W))
  val instr = Output(UInt(conf.DATA_WIDTH.W))
  val ex = Output(new CP0Exception)
}

class IDU_ISU_IO extends Bundle {
  val pc = Output(UInt(conf.DATA_WIDTH.W))
  val instr = Output(new Instr)
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1_sel = Output(UInt(OP1_SEL_SZ.W))
  val op2_sel = Output(UInt(OP2_SEL_SZ.W))
  val opd_sel = Output(UInt(OPD_SEL_SZ.W))
  val ex = Output(new CP0Exception)
}

class ISU_BRU_IN extends Bundle {
  val wb = Output(new WriteBackIO)
  val ops = Output(new EXU_OPS)
}

class ISU_BRU_OUT extends Bundle {
  val wb = Output(new WriteBackIO)
  val br_target = Output(UInt(conf.DATA_WIDTH.W))
}

class ISU_BRU_IO extends Bundle {
  val fu_in = Flipped(ValidIO(new ISU_BRU_IN))
  val fu_out = ValidIO(new ISU_BRU_OUT)
}

class ISU_EXU_IO extends Bundle {
  val wb = new WriteBackIO
  val ops = new EXU_OPS
  val ex = Output(new CP0Exception)
}

class EHU_MSU_IO extends Bundle {
  val wb = new WriteBackIO
  val ops = new EXU_OPS
  val is_cached = Output(Bool())
}

class EXU_EHU_IO extends EHU_MSU_IO {
  val ex = Output(new CP0Exception)
}

class EXU_OUT extends Bundle {
  val wb = new WriteBackIO
  val ex = Output(new CP0Exception)
}

class EXU_WBU_IO extends Bundle {
  val wb = new WriteBackIO
  val ex = Output(new CP0Exception)
}

class BRINFO_IO extends Bundle {
  val need_br = Output(Bool())
  val br_target = Output(UInt(conf.DATA_WIDTH.W))
}

class TLB_RPORT extends Bundle {
  val index = Output(UInt(log2Ceil(conf.TLBSZ).W))
  val entry = Input(new TLBEntry)
}

class TLB_WPORT extends Bundle {
  val index = Output(UInt(log2Ceil(conf.TLBSZ).W))
  val entry = Output(new TLBEntry)
}

class CPR_RPORT extends Bundle {
  val addr = Output(UInt(32.W))
  val data = Input(UInt(32.W))
}

class CPR_WPORT extends Bundle {
  val addr = Output(UInt(32.W))
  val data = Output(UInt(32.W))
}

class CP0_TLBR_PORT extends Bundle {
  val index     = Input(new CP0Index)
  val pagemask  = Input(new CP0PageMask)
  val entry_hi  = Input(new CP0EntryHI)
  val entry_lo0 = Input(new CP0EntryLO)
  val entry_lo1 = Input(new CP0EntryLO)
}

class CP0_TLBW_PORT extends Bundle {
  val pagemask  = Output(new CP0PageMask)
  val entry_hi  = Output(new CP0EntryHI)
  val entry_lo0 = Output(new CP0EntryLO)
  val entry_lo1 = Output(new CP0EntryLO)
}

class TLB_PPORT extends Bundle {
  val entry_hi = Output(new CP0EntryHI)
  val index = Input(new CP0Index)
}

class CP0_TLBP_PORT extends Bundle {
  val index = Output(new CP0Index)
}

class EHU_CP0_IO extends Bundle {
  val valid = Output(Bool())
  val ip7 = Input(Bool())
  val ex = Output(new CP0Exception)
  val wb = Output(new WriteBackIO)
}
