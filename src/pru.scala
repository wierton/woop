package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._

class PRU extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val fu_in = Flipped(DecoupledIO(new PRALU_FU_IO))
    val fu_out = DecoupledIO(new PRU_OUT_PRALU)
    val exinfo = Flipped(ValidIO(new CP0Exception))
    val ex_flush = ValidIO(new FlushIO)
  })

  /* handle memory translate request */
  val iaddr_req_valid = RegNext(io.iaddr.req.valid, init=N)
  val iaddr_req = RegNext(io.iaddr.req.bits)

  def naive_tlb_translate(addr:UInt) = Mux1H(Array(
    ("h00000000".U <= addr && addr < "h80000000".U) -> addr,
    ("h80000000".U <= addr && addr < "hA0000000".U) -> (addr - "h80000000".U),
    ("hA0000000".U <= addr && addr < "hC0000000".U) -> (addr - "hA0000000".U)))

  io.iaddr.req.ready := Y
  io.iaddr.resp.valid := iaddr_req_valid
  io.iaddr.resp.bits.paddr := naive_tlb_translate(iaddr_req.vaddr)
  io.iaddr.resp.bits.is_cached := iaddr_req.vaddr(31, 29) =/= "b101".U
  io.iaddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)

  /* pipeline stage for bru data */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val lsu_vaddr = fu_in.ops.op1
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb := fu_in.wb
  io.fu_out.bits.ops := fu_in.ops
  io.fu_out.bits.paddr := naive_tlb_translate(lsu_vaddr)
  io.fu_out.bits.is_cached := lsu_vaddr(31, 29) =/= "b101".U
  io.fu_out.bits.ex := 0.U.asTypeOf(new CP0Exception)

  /* exception flush */
  io.ex_flush.valid := N
  io.ex_flush.bits.br_target := 0.U

  /* PRU IO */
  io.fu_in.ready := io.fu_out.ready || !fu_valid
}
