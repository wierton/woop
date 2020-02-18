package MipsNPC
package IO

import chisel3._
import chisel3.util._

import Configure._
import Consts._

class Instr {
  val op     = UInt(OP_SZ.W)
  val rs_idx = UInt(REG_SZ.W)
  val rt_idx = UInt(REG_SZ.W)
  val rd_idx = UInt(REG_SZ.W)
  val shamt  = UInt(SHAMT_SZ.W)
  val func   = UInt(FUNC_SZ.W)

  def imm    = Cat(rd_idx, shamt, func)
  def addr   = Cat(rs_idx, rt_idx, imm)
}

class CP0Exception extends Bundle {
  val offset = UInt(12.W)
  val code = UInt(ETW_WIDTH.W)
}

class MemReq extends Bundle {
  val addr = Output(UInt(conf.xprlen.W)); // enable s
  val data  = Output(UInt(conf.xprlen.W))
  val func  = Output(UInt(MX_SZ.W))
  val wstrb = Output(UInt((conf.xprlen / 8).W))
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
  val rd = Output(UInt(REG_SZ.W))
  val wen = Output(Bool()); // need bypass
  val data = Output(UInt(conf.xprlen.W))
}

class WriteBackIO extends BypassIO {
  val npc = Output(UInt(conf.xprlen.W))
  val wen = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val data = Output(UInt(conf.xprlen.W))
}

class IFU_IDU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
}

class IDU_ISU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val op1_sel = Output(UInt(OP1_SEL_SZ.W))
  val op2_sel = Output(UInt(OP2_SEL_SZ.W))
  val dest_sel = Output(UInt(DEST_SEL_SZ.W))
}

//========================================================
//         ISU --> {ALU, LSU, MDU, BRU}
//========================================================
class ISU_ALU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val op1 = Output(UInt(conf.xprlen.W))
  val op2 = Output(UInt(conf.xprlen.W))
  val rd_idx = Output(UInt(REG_SZ.W))
}

class ISU_MDU_IO extends ISU_ALU_IO { }

class ISU_BRU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val npc = Output(UInt(conf.xprlen.W)); // PC next
  val rs_data = Output(UInt(conf.xprlen.W))
  val rt_data = Output(UInt(conf.xprlen.W))
  val addr = Output(UInt(ADDR_SZ.W))
  val se_off = Output(UInt(conf.xprlen.W))
  val rd_idx = Output(UInt(REG_SZ.W))
}

class ISU_LSU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W))
  val npc = Output(UInt(conf.xprlen.W))
  val base = Output(UInt(conf.xprlen.W))
  val offset = Output(UInt(conf.xprlen.W))
  val data = Output(UInt(conf.xprlen.W))
  val rd_idx = Output(UInt(REG_SZ.W))
}


//========================================================//
//        {ALU, LSU, MDU, BRU}  -->  WBU           //
//========================================================//
class LSU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_wb = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val data = Output(UInt(conf.xprlen.W))
}

class ALU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val rd_idx = Output(UInt(REG_SZ.W))
  val data = Output(UInt(conf.xprlen.W))
}

class MDU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_wb = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val data = Output(UInt(conf.xprlen.W))
}

class BRU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_br = Output(Bool())
  val br_target = Output(UInt(conf.xprlen.W))
  val need_wb = Output(Bool())
  val rd_idx = Output(UInt(REG_SZ.W))
  val data = Output(UInt(conf.xprlen.W))
}

