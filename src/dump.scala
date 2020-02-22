package njumips

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.utils._
import njumips.core._

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

  implicit class Decoupled_BRIDU_PRALU_IO_Dump(data:DecoupledIO[BRIDU_PRALU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]: fu_type=%d, fu_op=%d, op1_sel=%d, op2_sel=%d, rd_sel=%d\n", GTimer(), data.valid, data.bits.fu_type, data.bits.fu_op, data.bits.op1_sel, data.bits.op2_sel, data.bits.rd_sel)
      data.bits.wb.dump(msg+".wb")
    }
  }

  implicit class Decoupled_PRALU_LSMDU_IO_Dump(data:DecoupledIO[PRALU_LSMDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%b]: wb={rd:%d, wen:%b, data:%x}, ops={fu_type=%d, fu_op=%d, op1=%x, op2=%x}, paddr=%x, is_cached=%b\n", GTimer(), data.valid, data.bits.wb.rd_idx, data.bits.wb.wen, data.bits.wb.data, data.bits.ops.fu_type, data.bits.ops.fu_op, data.bits.ops.op1, data.bits.ops.op2, data.bits.paddr, data.bits.is_cached)
    }
  }

  implicit class RegFileReq_Dump(data:RegFileReq) {
    def dump(msg:String) = {
      printf("%d: "+msg+": rs_idx=%d, rt_idx=%d, rs[%d]=%x, rt[%d]=%x, rd[%b]=%x\n", GTimer(), data.rs_idx, data.rt_idx, data.rs_data.valid, data.rs_data.bits, data.rt_data.valid, data.rt_data.bits, data.dest_ridx.valid, data.dest_ridx.bits)
    }
  }

  implicit class IFU_BRIDU_IO_Dump(data:IFU_BRIDU_IO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": pc:%x, instr:%x\n", GTimer(), data.pc, data.instr.asUInt)
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
}

