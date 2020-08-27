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
  val addr = UInt(conf.DATA_WIDTH.W)
  val asid = UInt(8.W)
}

class RegFileIO extends Bundle {
  val rs_idx = Output(UInt(REG_SZ.W))
  val rt_idx = Output(UInt(REG_SZ.W))
  val wen = Output(Bool())
  val wid = Output(UInt(conf.INSTR_ID_SZ.W))
  val rd_idx = Output(UInt(REG_SZ.W))

  val rs_data = Flipped(ValidIO(Output(UInt(conf.DATA_WIDTH.W))))
  val rt_data = Flipped(ValidIO(Output(UInt(conf.DATA_WIDTH.W))))
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
  val vaddr = Output(UInt(conf.DATA_WIDTH.W))
  val len = Output(UInt(ML_SZ.W))
  val is_aligned = Output(Bool())
}

class TLBResp extends Bundle {
  val paddr = Output(UInt(conf.DATA_WIDTH.W))
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
  val addr = Output(UInt(conf.DATA_WIDTH.W))
  val len = Output(UInt(ML_SZ.W))                  // aligned
  val strb = Output(UInt((conf.DATA_WIDTH / 8).W)) // unaligned
  val data  = Output(UInt(conf.DATA_WIDTH.W))
  val func  = Output(UInt(MX_SZ.W))
  val id = Output(UInt(conf.INSTR_ID_SZ.W))
}

class MemResp extends Bundle {
  val data = Output(UInt(conf.DATA_WIDTH.W))
}

class MemIO extends Bundle {
  val req = DecoupledIO(new MemReq)
  val resp = Flipped(DecoupledIO(new MemResp))
}

class CommitIO extends Bundle {
  val valid = Output(Bool())
  val pc = Output(UInt(conf.DATA_WIDTH.W))
  val instr = Output(UInt(conf.DATA_WIDTH.W))
  val ip7 = Output(Bool())
  val gpr = Output(Vec(32, UInt(conf.DATA_WIDTH.W)))
  val rd_idx = Output(UInt(REG_SZ.W))
  val wdata = Output(UInt(conf.DATA_WIDTH.W))
  val wen = Output(Bool())
}

class NSCSCCCommitIO extends Bundle {
  val ninstr = Output(UInt(32.W))
  val wb_pc = Output(UInt(conf.DATA_WIDTH.W))
  val wb_instr = Output(UInt(conf.DATA_WIDTH.W))
  val wb_rf_wdata = Output(UInt(conf.DATA_WIDTH.W))
  val wb_rf_wen = Output(Bool())
  val wb_rf_wnum = Output(UInt(5.W))
  val wb_valid = Output(Bool())
}
