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

  val iaddr_req_valid = RegNext(io.iaddr.req.valid, init=N)
  val iaddr_req = RegNext(io.iaddr.req.bits)
  val daddr_req_valid = RegNext(io.daddr.req.valid, init=N)
  val daddr_req = RegNext(io.daddr.req.bits)

  def naive_tlb_translate(addr:UInt) = Mux1H(Array(
    ("h00000000".U <= addr && addr < "h80000000".U) -> addr,
    ("h80000000".U <= addr && addr < "hA0000000".U) -> (addr - "h80000000".U),
    ("hA0000000".U <= addr && addr < "hC0000000".U) -> (addr - "hA0000000".U)))

  io.iaddr.req.ready := Y
  io.iaddr.resp.valid := iaddr_req_valid
  io.iaddr.resp.bits.paddr := naive_tlb_translate(iaddr_req.vaddr)
  io.iaddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)

  io.daddr.req.ready := Y
  io.daddr.resp.valid := daddr_req_valid
  io.daddr.resp.bits.paddr := naive_tlb_translate(daddr_req.vaddr)
  io.daddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)
}
