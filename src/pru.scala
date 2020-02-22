package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._


class PRU extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val daddr = Flipped(new TLBTransaction)
    val fu_in = Flipped(DecoupledIO(new EXU_IO))
    val fu_out = DecoupledIO(new EXU_IO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
  })

  /* handle memory translate request */
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
  io.iaddr.resp.bits.is_cached := iaddr_req.vaddr(31, 29) =/= "b101".U
  io.iaddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)

  io.daddr.req.ready := Y
  io.daddr.resp.valid := daddr_req_valid
  io.daddr.resp.bits.paddr := naive_tlb_translate(daddr_req.vaddr)
  io.daddr.resp.bits.is_cached := daddr_req.vaddr(31, 29) =/= "b101".U
  io.daddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)


  /* pipeline stage for bru data */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
  io.fu_out.valid := fu_valid
  io.fu_out.bits := fu_in
}
