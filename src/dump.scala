package woop

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.core._

object dumps {
  implicit class Instr_Dump(data:Instr) {
    def dump(msg:String) = {
      printf("%d: "+msg+": instr[%x]={func:%d, shamt:%d, rd:%d, rt:%d, rs:%d, op:%d, imm:%x, addr:%x}\n", GTimer(), data.asUInt, data.func, data.shamt, data.rd_idx, data.rt_idx, data.rs_idx, data.op, data.imm, data.addr)
    }
  }

  implicit class WritebackIO_Dump(data:WriteBackIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": pc=%x, instr=%x, rd=%d, wen=%d, data=%x\n", GTimer(), data.pc, data.instr.asUInt, data.rd_idx, data.wen, data.data)
    }
  }

  implicit class Decoupled_EXU_OUT_Dump(data:DecoupledIO[EXU_OUT]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]: wb={v:%b, id:%x, pc:%x, instr:%x, rd:%d, wen:%b, data:%x}, et=%d, code=%d\n", GTimer(), data.valid, data.ready, data.bits.wb.v, data.bits.wb.id, data.bits.wb.pc, data.bits.wb.instr.asUInt, data.bits.wb.rd_idx, data.bits.wb.wen, data.bits.wb.data, data.bits.ex.et, data.bits.ex.code)
    }
  }

  implicit class Decoupled_ISU_EXU_IO_Dump(data:DecoupledIO[ISU_EXU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]: wb={v:%b, id=%x, pc:%x, instr:%x, rd:%d, wen:%b, data:%x}, ops={fu_type=%d, fu_op=%d, op1=%x, op2=%x}, et=%d, code=%d\n", GTimer(), data.valid, data.ready, data.bits.wb.v, data.bits.wb.id, data.bits.wb.pc, data.bits.wb.instr.asUInt, data.bits.wb.rd_idx, data.bits.wb.wen, data.bits.wb.data, data.bits.ops.fu_type, data.bits.ops.fu_op, data.bits.ops.op1, data.bits.ops.op2, data.bits.ex.et, data.bits.ex.code)
    }
  }

  implicit class Decoupled_EXU_LSMDU_IO_Dump(data:DecoupledIO[EXU_LSMDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]: wb={v:%b, id:%x, pc:%x, instr:%x, rd:%d, wen:%b, data:%x}, ops={fu_type=%d, fu_op=%d, op1=%x, op2=%x}, paddr=%x, is_cached=%b, et=%d, code=%d\n", GTimer(), data.valid, data.ready, data.bits.wb.v, data.bits.wb.id, data.bits.wb.pc, data.bits.wb.instr.asUInt, data.bits.wb.rd_idx, data.bits.wb.wen, data.bits.wb.data, data.bits.ops.fu_type, data.bits.ops.fu_op, data.bits.ops.op1, data.bits.ops.op2, data.bits.paddr, data.bits.is_cached, data.bits.ex.et, data.bits.ex.code)
    }
  }

  implicit class RegFileIO_Dump(data:RegFileIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": rs_idx=%d, rt_idx=%d, wid=%x, rd[%b]=%x, rs[%d]=%x, rt[%d]=%x\n", GTimer(), data.rs_idx, data.rt_idx, data.wid, data.wen, data.rd_idx, data.rs_data.valid, data.rs_data.bits, data.rt_data.valid, data.rt_data.bits)
    }
  }

  implicit class IFU_IDU_IO_Dump(data:IFU_IDU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": pc:%x, instr:%x, et:%d, code:%d\n", GTimer(), data.pc, data.instr.asUInt, data.ex.et, data.ex.code)
    }
  }

  implicit class Decoupled_IFU_IDU_IO_Dump(data:DecoupledIO[IFU_IDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x, instr:%x, et:%d, code:%d}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr, data.bits.ex.et, data.bits.ex.code)
    }
  }

  implicit class Decoupled_IDU_ISU_IO_Dump(data:DecoupledIO[IDU_ISU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x, instr:%x, fu_type=%d, fu_op=%d, op1_sel=%d, op2_sel=%d, opd_sel=%d, et=%, ec=%d}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr.asUInt, data.bits.fu_type, data.bits.fu_op, data.bits.op1_sel, data.bits.op2_sel, data.bits.opd_sel, data.bits.ex.et, data.bits.ex.code)
    }
  }

  implicit class MemReq_Dump(data:MemReq) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req={align:%d, addr:%x, len:%d, data:%x, func:%d, strb:%x}\n", GTimer(), data.is_aligned, data.addr, data.len, data.data, data.func, data.strb)
    }
  }

  implicit class Decoupled_MemResp_Dump(data:DecoupledIO[MemResp]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": resp[%d,%d]={data:%x}\n", GTimer(), data.valid, data.ready, data.bits.data)
    }
  }

  implicit class MemIO_Dump(data:MemIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={align:%d, addr:%x, len:%d, data:%x, func:%d, strb:%x}, resp[%d,%d]={data:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.is_aligned, data.req.bits.addr, data.req.bits.len, data.req.bits.data, data.req.bits.func, data.req.bits.strb, data.resp.valid, data.resp.ready, data.resp.bits.data)
    }
  }

  implicit class Valid_FlushIO_Dump(data:ValidIO[FlushIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d]={br_target:%x}\n", GTimer(), data.valid, data.bits.br_target)
    }
  }

  implicit class TLBTransaction_Dump(data:TLBTransaction) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={func:%d, vaddr:%x}, resp[%b,%b]={paddr:%x, ex:%d.%d}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.func, data.req.bits.vaddr, data.resp.valid, data.resp.ready, data.resp.bits.paddr, data.resp.bits.ex.et, data.resp.bits.ex.code)
    }
  }

  implicit class Valid_BYPASS_IO_Dump(data:ValidIO[BypassIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={v:%b, rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.v, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class Valid_WriteBack_IO_Dump(data:ValidIO[WriteBackIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={v:%b, id:%x, pc:%x, instr:%x, rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.v, data.bits.id, data.bits.pc, data.bits.instr.asUInt, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class LSUStage2Data_Dump(data:LSUStage2Data) {
    def dump(msg:String) = {
      printf("%d: "+msg+": op=%b, instr=%x, rd=%d, pc=%x, data=%x, addr=%x, cache=%b\n", GTimer(), data.op.asUInt, data.instr.asUInt, data.rd_idx, data.pc, data.data, data.addr, data.is_cached)
    }
  }

  implicit class Decoupled_LSUStage2Data_Dump(data:DecoupledIO[LSUStage2Data]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={op:%b, instr:%x, rd:%d, pc:%x, data:%x, addr:%x, cache:%b}\n", GTimer(), data.valid, data.ready, data.bits.op.asUInt, data.bits.instr.asUInt, data.bits.rd_idx, data.bits.pc, data.bits.data, data.bits.addr, data.bits.is_cached)
    }
  }
}

