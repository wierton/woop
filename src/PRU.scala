package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class TLBRequest extends Bundle {
  val func = Output(Bool()) // 1: load, 0:store
  val vaddr = Output(UInt(conf.xprlen.W))
}

class TLBResponse extends Bundle {
  val paddr = Output(UInt(conf.xprlen.W))
  val ex = Output(new CP0Exception)
}

class TLBTransaction extends Bundle {
  // when tlbx is executing, the req should be hanged
  val req = DecoupledIO(new TLBRequest)
  val resp = Flipped(ValidIO(new TLBResponse))
}

class PRU extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val daddr = Flipped(new TLBTransaction)
  })

  val iaddr = RegNext(io.iaddr)
  val daddr = RegNext(io.daddr)

  def naive_tlb_translate(addr:UInt) = Mux1H(
    ("h00000000".U <= addr && addr < "h80000000".U) -> addr,
    ("h80000000".U <= addr && addr < "hA0000000".U) -> (addr - "h80000000".U),
    ("hA0000000".U <= addr && addr < "hC0000000".U) -> (addr - "hA0000000".U)
  )

  io.iaddr.resp.valid := iaddr.req.valid
  io.iaddr.resp.bits.paddr := naive_tlb_translate(iaddr.req.vaddr)
  io.iaddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)

  io.daddr.resp.valid := daddr.req.valid
  io.daddr.resp.bits.paddr := naive_tlb_translate(daddr.req.vaddr)
  io.daddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)
}
