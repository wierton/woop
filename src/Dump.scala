package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

object DumpUtils {
  implicit class IFU_IDU_IO_DUMP(data:IFU_IDU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": {pc:%x, instr:%x}\n", GTimer(), data.pc, data.instr)
    }
  }

  implicit class Decoupled_IFU_IDU_IO_DUMP(data:DecoupledIO[IFU_IDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x, instr:%x}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr)
    }
  }

  implicit class MemReq_DUMP(data:MemReq) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req={align:%d, addr:%x, data:%x, func:%d, strb:%x}\n", GTimer(), data.is_aligned, data.addr, data.data, data.func, data.wstrb)
    }
  }

  implicit class MemIO_DUMP(data:MemIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={align:%d, addr:%x, data:%x, func:%d, strb:%x}, resp[%d,%d]={data:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.is_aligned, data.req.bits.addr, data.req.bits.data, data.req.bits.func, data.req.bits.wstrb, data.resp.valid, data.resp.ready, data.resp.bits.data)
    }
  }

  implicit class FlushIO_DUMP(data:ValidIO[FlushIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d]={br_target:%x}\n", GTimer(), data.valid, data.bits.br_target)
    }
  }

  implicit class TLBTransaction_DUMP(data:TLBTransaction) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={func:%d, vaddr:%x}, resp[%d]={paddr:%x, ex:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.func, data.req.bits.vaddr, data.resp.valid, data.resp.bits.paddr, data.resp.bits.ex.asUInt)
    }
  }

  implicit class IDU_ISU_IO_DUMP(data:IDU_ISU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": pc:%x,instr:%x,fu_type:%d,fu_op:%d,op1_sel:%d,op2_sel:%d,rd:%d\n", GTimer(), data.pc, data.instr, data.fu_type, data.fu_op, data.op1_sel, data.op2_sel, data.rd_sel)
    }
  }

  implicit class Decoupled_IDU_ISU_IO_DUMP(data:DecoupledIO[IDU_ISU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x,instr:%x,fu_type:%d,fu_op:%d,op1_sel:%d,op2_sel:%d,rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr, data.bits.fu_type, data.bits.fu_op, data.bits.op1_sel, data.bits.op2_sel, data.bits.rd_sel)
    }
  }

  implicit class VALID_BYPASS_IO_DUMP(data:ValidIO[BypassIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class VALID_WriteBack_IO_DUMP(data:ValidIO[WriteBackIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={pc:%x, rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.pc, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class Decoupled_ISU_ALU_IO_DUMP(data:DecoupledIO[ISU_ALU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, op1:%x, op2:%x, rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.op1, data.bits.op2, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_MDU_IO_DUMP(data:DecoupledIO[ISU_MDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, op1:%x, op2:%x, rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.op1, data.bits.op2, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_BRU_IO_DUMP(data:DecoupledIO[ISU_BRU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, rs:%x, rt:%x, addr:%x, se_off:%x, rd:%d\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.rs_data, data.bits.rt_data, data.bits.addr, data.bits.se_off, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_LSU_IO_DUMP(data:DecoupledIO[ISU_LSU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, base:%x, offset:%x, data:%x, rd:%d\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.base, data.bits.offset, data.bits.data, data.bits.rd_idx)
    }
  }
}

