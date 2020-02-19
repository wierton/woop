package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

object DumpUtils {
  implicit class IFU_IDU_IO_DUMP(data:DecoupledIO[IFU_IDU_IO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d]={npc:%x, instr:%x}\n", GTimer(), data.valid, data.bits.npc, data.bits.instr)
    }
  }

  implicit class MemReq_DUMP(data:MemReq) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req={align:%d, addr:%x, data:%x, func:%d, strb:%x}\n", GTimer(), data.is_aligned, data.addr, data.data, data.func, data.wstrb)
    }
  }

  implicit class MemIO_DUMP(data:MemIO) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={align:%d, addr:%x, data:%x, func:%d, strb:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.is_aligned, data.req.bits.addr, data.req.bits.data, data.req.bits.func, data.req.bits.wstrb)
      printf("%d: "+msg+": resp[%d,%d]={data:%x}\n", GTimer(), data.resp.valid, data.resp.ready, data.resp.bits.data)
    }
  }

  implicit class FlushIO_DUMP(data:ValidIO[FlushIO]) {
    def dump(msg:String) = {
      printf("%d: "+msg+": [%d]={br_target:%x}\n", GTimer(), data.valid, data.bits.br_target)
    }
  }

  implicit class TLBTransaction_DUMP(data:TLBTransaction) {
    def dump(msg:String) = {
      printf("%d: "+msg+": req[%d,%d]={func:%d, vaddr:%x}\n", GTimer(), data.req.valid, data.req.ready, data.req.bits.func, data.req.bits.vaddr)
      printf("%d: "+msg+": resp[%d]={paddr:%x, ex:%x}\n", GTimer(), data.resp.valid, data.resp.bits.paddr, data.resp.bits.ex.asUInt)
    }
  }
}

