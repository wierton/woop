package woop
package core

import chisel3._
import chisel3.util._
import woop.configs._
import woop.consts._

class Instr extends Bundle {
  val op     = UInt(6.W)
  val rs_idx = UInt(REG_SZ.W)
  val rt_idx = UInt(REG_SZ.W)
  val rd_idx = UInt(REG_SZ.W)
  val shamt  = UInt(5.W)
  val func   = UInt(6.W)

  def imm    = Cat(rd_idx, shamt, func)
  def simm   = imm.asTypeOf(SInt(32.W))
  def uimm   = imm.asTypeOf(UInt(32.W))
  def addr   = Cat(rs_idx, rt_idx, imm)
  def sel    = func(2, 0)
  def lsb    = shamt
  def msb    = rd_idx
}

class CacheControl extends Bundle {
  val op = UInt(CACHE_OP_SZ.W)
  val addr = UInt(32.W)
}

class CP0Exception extends Bundle {
  val et = UInt(ET_WIDTH.W)
  val code = UInt(EC_WIDTH.W)
  val addr = UInt(conf.xprlen.W)
  val asid = UInt(8.W)
}

class RegFileIO extends Bundle {
  val rs_idx = Output(UInt(REG_SZ.W))
  val rt_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val wid = Output(UInt(conf.INSTR_ID_SZ.W))
  val rd_idx = Output(UInt(REG_SZ.W))

  val rs_data = Flipped(ValidIO(Output(UInt(conf.xprlen.W))))
  val rt_data = Flipped(ValidIO(Output(UInt(conf.xprlen.W))))
}

class DividerIO extends Bundle {
  val data_dividend_tvalid = Output(Bool())
  val data_divisor_tvalid = Output(Bool())
  val data_dout_tvalid = Input(Bool())
  val data_dividend_tready = Input(Bool())
  val data_divisor_tready = Input(Bool())
  val data_dividend_tdata = Output(UInt(40.W))
  val data_divisor_tdata = Output(UInt(40.W))
  val data_dout_tdata = Input(UInt(80.W))

  def dividend_fire() = data_dividend_tvalid && data_dividend_tready
  def divisor_fire() = data_divisor_tvalid && data_divisor_tready
}

class MultiplierIO extends Bundle {
  val data_a = Output(UInt(33.W))
  val data_b = Output(UInt(33.W))
  val data_dout = Input(UInt(66.W))
}

class TLBReq extends Bundle {
  val func = Output(Bool()) // 1: load, 0:store
  val vaddr = Output(UInt(conf.xprlen.W))
  val len = Output(UInt(ML_SZ.W))
  val is_aligned = Output(Bool())
}

class TLBResp extends Bundle {
  val paddr = Output(UInt(conf.xprlen.W))
  val is_cached = Output(Bool())
  val ex = Output(new CP0Exception)
}

class TLBTransaction extends Bundle {
  // when tlbx is executing, the req should be hanged
  val req = DecoupledIO(new TLBReq)
  val resp = Flipped(DecoupledIO(new TLBResp))
}

class MemReq extends Bundle {
  val is_cached = Output(Bool())
  val addr = Output(UInt(conf.xprlen.W))
  val len = Output(UInt(ML_SZ.W))              // aligned
  val strb = Output(UInt((conf.xprlen / 8).W)) // unaligned
  val data  = Output(UInt(conf.xprlen.W))
  val func  = Output(UInt(MX_SZ.W))
}

class MemResp extends Bundle {
  val data = Output(UInt(conf.xprlen.W))
}

class MemIO extends Bundle {
  val req = DecoupledIO(new MemReq)
  val resp = Flipped(DecoupledIO(new MemResp))
}

trait MemoryMapped {
  def io_in = Flipped(new MemIO)
  val start_addr:Int = 0
  val end_addr:Int = 0
}

class CommitIO extends Bundle {
  val valid = Output(Bool())
  val pc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
  val ip7 = Output(Bool())
  val gpr = Output(Vec(32, UInt(conf.xprlen.W)))
  val rd_idx = Output(UInt(REG_SZ.W))
  val wdata = Output(UInt(conf.xprlen.W))
  val wen = Output(Bool())
}

class FlushIO extends Bundle {
  val br_target = Output(UInt(conf.xprlen.W))
}

class BypassIO extends Bundle {
  val v = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val data = Output(UInt(conf.xprlen.W))
}

class WriteBackIO extends Bundle {
  val v = Output(Bool())
  val id = Output(UInt(conf.INSTR_ID_SZ.W))
  val pc = Output(UInt(conf.xprlen.W))
  val instr = Output(new Instr)
  val rd_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val data = Output(UInt(conf.xprlen.W))
  val ip7 = Output(Bool())
  val is_ds = Output(Bool()) // is_delayslot
  val is_br = Output(Bool())
  val npc = Output(UInt(conf.xprlen.W))
}

class EXU_OPS extends Bundle {
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(conf.xprlen.W))
  val op2 = Output(UInt(conf.xprlen.W))
}

class IFU_IDU_IO extends Bundle {
  val pc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
  val ex = Output(new CP0Exception)
}

class IDU_ISU_IO extends Bundle {
  val pc = Output(UInt(conf.xprlen.W))
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
  val br_target = Output(UInt(conf.xprlen.W))
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
  val br_target = Output(UInt(conf.xprlen.W))
}

class TLB_RPORT extends Bundle {
  val index = Output(UInt(log2Ceil(conf.tlbsz).W))
  val entry = Input(new TLBEntry)
}

class TLB_WPORT extends Bundle {
  val index = Output(UInt(log2Ceil(conf.tlbsz).W))
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

class NSCSCCCommitIO extends Bundle {
  val ninstr = Output(UInt(32.W))
  val wb_pc = Output(UInt(conf.xprlen.W))
  val wb_instr = Output(UInt(conf.xprlen.W))
  val wb_rf_wdata = Output(UInt(conf.xprlen.W))
  val wb_rf_wen = Output(Bool())
  val wb_rf_wnum = Output(UInt(5.W))
  val wb_valid = Output(Bool())
}
