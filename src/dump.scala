package njumips

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.utils._
import njumips.core._

object dumps {
  implicit class IFU_BRIDU_IO_Dump(data:IFU_BRIDU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": {pc:%x, instr:%x}\n", GTimer(), data.pc, data.instr)
    }
  }

  implicit class Decoupled_IFU_BRIDU_IO_Dump(data:DecoupledIO[IFU_BRIDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x, instr:%x}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr)
    }
  }

  implicit class MemReq_Dump(data:MemReq) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req={align:%d, addr:%x, data:%x, func:%d, strb:%x}\n", GTimer(), data.is_aligned, data.addr, data.data, data.func, data.wstrb)
    }
  }

  implicit class Decoupled_MemResp_Dump(data:DecoupledIO[MemResp]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": resp[%d,%d]={data:%x}\n", GTimer(), data.valid, data.ready, data.bits.data)
    }
  }

  implicit class MemIO_Dump(data:MemIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={align:%d, addr:%x, data:%x, func:%d, strb:%x}, resp[%d,%d]={data:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.is_aligned, data.req.bits.addr, data.req.bits.data, data.req.bits.func, data.req.bits.wstrb, data.resp.valid, data.resp.ready, data.resp.bits.data)
    }
  }

  implicit class Valid_FlushIO_Dump(data:ValidIO[FlushIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d]={br_target:%x}\n", GTimer(), data.valid, data.bits.br_target)
    }
  }

  implicit class TLBTransaction_Dump(data:TLBTransaction) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={func:%d, vaddr:%x}, resp[%d]={paddr:%x, ex:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.func, data.req.bits.vaddr, data.resp.valid, data.resp.bits.paddr, data.resp.bits.ex.asUInt)
    }
  }

  implicit class BRIDU_ISU_IO_Dump(data:BRIDU_ISU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": pc:%x,instr:%x,fu_type:%d,fu_op:%d,op1_sel:%d,op2_sel:%d,rd:%d\n", GTimer(), data.pc, data.instr, data.fu_type, data.fu_op, data.op1_sel, data.op2_sel, data.rd_sel)
    }
  }

  implicit class Decoupled_BRIDU_ISU_IO_Dump(data:DecoupledIO[BRIDU_ISU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d,%d]={pc:%x,instr:%x,fu_type:%d,fu_op:%d,op1_sel:%d,op2_sel:%d,rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.pc, data.bits.instr, data.bits.fu_type, data.bits.fu_op, data.bits.op1_sel, data.bits.op2_sel, data.bits.rd_sel)
    }
  }

  implicit class Valid_BYPASS_IO_Dump(data:ValidIO[BypassIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class Valid_WriteBack_IO_Dump(data:ValidIO[WriteBackIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]={pc:%x, rd:%d, wen:%b, data:%x}\n", GTimer(), data.valid, data.bits.pc, data.bits.rd_idx, data.bits.wen, data.bits.data)
    }
  }

  implicit class Decoupled_ISU_ALU_IO_Dump(data:DecoupledIO[ISU_ALU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, op1:%x, op2:%x, rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.op1, data.bits.op2, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_MDU_IO_Dump(data:DecoupledIO[ISU_MDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, op1:%x, op2:%x, rd:%d}\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.op1, data.bits.op2, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_BRU_IO_Dump(data:DecoupledIO[ISU_BRU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, rs:%x, rt:%x, addr:%x, se_off:%x, rd:%d\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.rs_data, data.bits.rt_data, data.bits.addr, data.bits.se_off, data.bits.rd_idx)
    }
  }

  implicit class Decoupled_ISU_LSU_IO_Dump(data:DecoupledIO[ISU_LSU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b,%b]={fu_op:%d, pc:%x, base:%x, offset:%x, data:%x, rd:%d\n", GTimer(), data.valid, data.ready, data.bits.fu_op, data.bits.pc, data.bits.base, data.bits.offset, data.bits.data, data.bits.rd_idx)
    }
  }
}

