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
}

class CP0Exception extends Bundle {
  val et = UInt(ET_WIDTH.W)
  val code = UInt(EC_WIDTH.W)
}

class CP0ExInfo extends Bundle {
  val ex = new CP0Exception
  val is_ds = Bool()
  val pc = UInt(conf.xprlen.W)
  val addr = UInt(conf.xprlen.W)
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

class TLBReq extends Bundle {
  val func = Output(Bool()) // 1: load, 0:store
  val vaddr = Output(UInt(conf.xprlen.W))
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
  val is_aligned = Output(Bool())
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
  val gpr = Output(Vec(32, UInt(conf.xprlen.W)))
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
  val is_ds = Output(Bool()) // is_delayslot
}

class IFU_BRIDU_IO extends Bundle {
  val pc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
  val ex = Output(new CP0Exception)
}

class EXU_OPS extends Bundle {
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1 = Output(UInt(conf.xprlen.W))
  val op2 = Output(UInt(conf.xprlen.W))
}

class BRIDU_PRALU_IO extends Bundle {
  val wb = new WriteBackIO
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1_sel = Output(UInt(OP1_SEL_SZ.W))
  val op2_sel = Output(UInt(OP2_SEL_SZ.W))
  val opd_sel = Output(UInt(OPD_SEL_SZ.W))
  val ex = Output(new CP0Exception)
}

class PRALU_FU_IO extends Bundle {
  val wb = new WriteBackIO
  val ops = new EXU_OPS
  val ex = Output(new CP0Exception)
}

class PRALU_LSMDU_IO extends Bundle {
  val wb = new WriteBackIO
  val ops = new EXU_OPS
  val ex = Output(new CP0Exception)
  val paddr = Output(UInt(conf.xprlen.W))
  val is_cached = Output(Bool())
}

class PRU_OUT_PRALU extends PRALU_LSMDU_IO {
  val addr = Output(UInt(conf.xprlen.W))
}

class PRALU_OUT extends Bundle {
  val wb = new WriteBackIO
  val ex = Output(new CP0Exception)
}

class ISU_LSU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val pc = Output(UInt(conf.xprlen.W))
  val base = Output(UInt(conf.xprlen.W))
  val offset = Output(UInt(conf.xprlen.W))
  val data = Output(UInt(conf.xprlen.W))
  val rd_idx = Output(UInt(REG_SZ.W))
}

class EXU_WBU_IO extends Bundle {
  val wb = new WriteBackIO
  val ex = Output(new CP0Exception)
}

class BRINFO_IO extends Bundle {
  val need_br = Output(Bool())
  val br_target = Output(UInt(conf.xprlen.W))
}

