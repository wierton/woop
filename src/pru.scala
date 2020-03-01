package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._

abstract class CPRS extends Module {
  val index     = Reg(new CP0Index)
  def random    = Reg(new CP0Random)
  val entry_lo0 = Reg(new CP0EntryLO)
  val entry_lo1 = Reg(new CP0EntryLO)
  val entry_hi  = Reg(new CP0EntryHI)
  val context   = Reg(new CP0Context)
  val pagemask  = Reg(new CP0PageMask)
  val wired     = Reg(new CP0Wired)
  val base      = RegInit(0.U(32.W))    // 7, 0
  val badvaddr  = RegInit(0.U(32.W))    // 8, 0
  val count0    = RegInit(1.U(32.W))    // 9, 0
  val count1    = RegInit(0.U(32.W))    // 9, 1
  val compare   = RegInit(~(0.U(32.W))) // 11, 0
  val status    = Reg(new CP0Status) // 12, 0
  val cause     = Reg(new CP0Cause)  // 13, 0
  val epc       = RegInit(0.U(32.W))       // 14, 0
  val prid      = Reg(new CP0Prid)
  val config    = Reg(new CP0Config)
  val config1   = Reg(new CP0Config1)
  when(reset.toBool) {
    index.init()
    random.init()
    entry_lo0.init()
    entry_lo1.init()
    context.init()
    pagemask.init()
    wired.init()
    status.init()
    cause.init()
    prid.init()
    config.init()
    config1.init()
  }
}

class PRU extends CPRS {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val fu_in = Flipped(DecoupledIO(new PRALU_FU_IO))
    val fu_out = DecoupledIO(new PRU_OUT_PRALU)
    val exinfo = Flipped(ValidIO(new CP0Exception))
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = ValidIO(new FlushIO)
  })

  def is_cached(vaddr:UInt) = vaddr(31, 29) =/= "b101".U

  def naive_tlb_translate(addr:UInt) = Mux1H(Array(
    ("h00000000".U <= addr && addr < "h80000000".U) -> addr,
    ("h80000000".U <= addr && addr < "hA0000000".U) -> (addr - "h80000000".U),
    ("hA0000000".U <= addr && addr < "hC0000000".U) -> (addr - "hA0000000".U)))

  /* handle memory translate request, a pipeline stage */
  val iaddr_in = RegEnable(io.iaddr.req.bits, enable=io.iaddr.req.fire())
  val iaddr_valid = RegInit(N)
  val flush_valid = io.br_flush.valid || io.ex_flush.valid
  io.iaddr.req.ready := io.iaddr.resp.ready || !iaddr_valid
  io.iaddr.resp.valid := iaddr_valid
  io.iaddr.resp.bits.paddr := naive_tlb_translate(iaddr_in.vaddr)
  io.iaddr.resp.bits.is_cached := is_cached(iaddr_in.vaddr)
  io.iaddr.resp.bits.ex := 0.U.asTypeOf(new CP0Exception)
  when (flush_valid || (!io.iaddr.req.fire() && io.iaddr.resp.fire())) {
    iaddr_valid := N
  } .elsewhen (!flush_valid && io.iaddr.req.fire()) {
    iaddr_valid := Y
  }

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
  io.fu_out.bits.ex.et := Mux(
    fu_in.ops.fu_type === FU_PRU && fu_in.ex.et === ET_None,
    Mux1H(Array(
      (fu_in.ops.fu_op === PRU_SYSCALL)-> ET_Sys,
      (fu_in.ops.fu_op === PRU_BREAK)  -> ET_Bp)),
    fu_in.ex.et)
  io.fu_out.bits.ex.code := Mux(
    fu_in.ops.fu_type === FU_PRU && fu_in.ex.et === EC_None,
    Mux1H(Array(
      (fu_in.ops.fu_op === PRU_SYSCALL)-> EC_Sys,
      (fu_in.ops.fu_op === PRU_BREAK)  -> EC_Bp)),
    fu_in.ex.code)
    
  /* exception flush */
  io.ex_flush.valid := io.exinfo.valid && io.exinfo.bits.et =/= ET_None && io.exinfo.bits.code =/= EC_None
  io.ex_flush.bits.br_target := "hbfc00380".U

  /* PRU IO */
  io.fu_in.ready := io.fu_out.ready || !fu_valid
}
