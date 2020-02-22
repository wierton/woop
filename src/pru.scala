package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._

class PRU extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val fu_in = Flipped(DecoupledIO(new PRIDU_IN))
    val pru_out = DecoupledIO(new PRIDU_LSMDU_IO)
    val exinfo = Flipped(ValidIO(new CP0Expcetion))
    val ex_flush = ValidIO(new FlushIO)
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

  /* pipeline stage for bru data */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val se_imm = fu_in.wb.instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val lsu_vaddr = fu_in.op1 + se_imm
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.pru_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
  io.pru_out.valid := fu_valid
  io.pru_out.bits.fu_out := fu_in
  io.pru_out.bits.paddr := naive_tlb_translate(lsu_vaddr)
  io.pru_out.bits.is_cached := lsu_vaddr(31, 29) =/= "b101".U

  /* exception flush */
  // io.ex_flush.valid := io.fu_in.ex || xx
}
