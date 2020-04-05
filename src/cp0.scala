package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._


abstract class CPRS extends Module {
  val cpr_index     = Reg(new CP0Index)
  val cpr_entry_lo0 = Reg(new CP0EntryLO)
  val cpr_entry_lo1 = Reg(new CP0EntryLO)
  val cpr_context   = Reg(new CP0Context)
  val cpr_pagemask  = Reg(new CP0PageMask)
  val cpr_wired     = Reg(new CP0Wired)
  val cpr_base      = RegInit(0.U(32.W))    // 7, 0
  val cpr_badvaddr  = RegInit(0.U(32.W))    // 8, 0
  val cpr_count     = RegInit(1.U(32.W))    // 9, 0
  val cpr_entry_hi  = Reg(new CP0EntryHI)
  val cpr_compare   = RegInit(~(0.U(32.W))) // 11, 0
  val cpr_status    = Reg(new CP0Status) // 12, 0
  val cpr_cause     = Reg(new CP0Cause)  // 13, 0
  val cpr_epc       = RegInit(0.U(32.W))       // 14, 0
  val cpr_prid      = Reg(new CP0Prid)
  val cpr_ebase     = RegInit(0.U(32.W))
  val cpr_config    = Reg(new CP0Config)
  val cpr_config1   = Reg(new CP0Config1)
  when(reset.toBool) {
    cpr_index.init()
    cpr_entry_lo0.init()
    cpr_entry_lo1.init()
    cpr_context.init()
    cpr_pagemask.init()
    cpr_wired.init()
    cpr_status.init()
    cpr_cause.init()
    cpr_prid.init()
    cpr_config.init()
    cpr_config1.init()
  }
  cpr_count := cpr_count + 1.U
}

class CP0 extends CPRS with LSUConsts {
  val io = IO(new Bundle {
    val rport = Flipped(new CPR_RPORT)
    val wport = Flipped(ValidIO(new CPR_WPORT))
    val tlbr_port = Flipped(new CP0_TLBR_PORT)
    val tlbw_port = Flipped(ValidIO(new CP0_TLBW_PORT))
    val tlbp_port = Flipped(ValidIO(new CP0_TLBP_PORT))
    val status = Output(new CP0Status)
    val ehu = Flipped(new EHU_CP0_IO)
    val ex_flush = ValidIO(new FlushIO)
    val can_log_now = Input(Bool())
  })

  io.status := cpr_status

  /* cpr io */
  val cpr_raddr = io.rport.addr
  io.rport.data := Mux1H(Array(
    (cpr_raddr === CPR_INDEX)     -> cpr_index.asUInt,
    (cpr_raddr === CPR_ENTRY_LO0) -> cpr_entry_lo0.asUInt,
    (cpr_raddr === CPR_ENTRY_LO1) -> cpr_entry_lo1.asUInt,
    (cpr_raddr === CPR_CONTEXT)   -> cpr_context.asUInt,
    (cpr_raddr === CPR_PAGEMASK)  -> cpr_pagemask.asUInt,
    (cpr_raddr === CPR_WIRED)     -> cpr_wired.asUInt,
    (cpr_raddr === CPR_BAD_VADDR) -> cpr_badvaddr.asUInt,
    (cpr_raddr === CPR_COUNT)     -> cpr_count.asUInt,
    (cpr_raddr === CPR_ENTRY_HI)  -> cpr_entry_hi.asUInt,
    (cpr_raddr === CPR_COMPARE)   -> cpr_compare.asUInt,
    (cpr_raddr === CPR_STATUS)    -> cpr_status.asUInt,
    (cpr_raddr === CPR_CAUSE)     -> cpr_cause.asUInt,
    (cpr_raddr === CPR_EPC)       -> cpr_epc.asUInt,
    (cpr_raddr === CPR_PRID)      -> cpr_prid.asUInt,
    (cpr_raddr === CPR_EBASE)     -> cpr_ebase.asUInt,
    (cpr_raddr === CPR_CONFIG)    -> cpr_config.asUInt,
    (cpr_raddr === CPR_CONFIG1)   -> cpr_config1.asUInt,
  ))

  val cpr_wdata = io.wport.bits.data
  when (io.wport.valid && !io.ex_flush.valid) {
    switch (io.wport.bits.addr) {
    is(CPR_INDEX)     { cpr_index.write(cpr_wdata) }
    is(CPR_ENTRY_LO0) { cpr_entry_lo0.write(cpr_wdata) }
    is(CPR_ENTRY_LO1) { cpr_entry_lo1.write(cpr_wdata) }
    is(CPR_CONTEXT)   { cpr_context.write(cpr_wdata) }
    is(CPR_PAGEMASK)  { cpr_pagemask.write(cpr_wdata) }
    is(CPR_WIRED)     { cpr_wired.write(cpr_wdata) }
    is(CPR_BAD_VADDR) { cpr_badvaddr := cpr_wdata }
    is(CPR_COUNT)     { }
    is(CPR_ENTRY_HI)  { cpr_entry_hi.write(cpr_wdata) }
    is(CPR_COMPARE)   {
      cpr_compare := cpr_wdata
      cpr_cause.IP(7) := N
    }
    is(CPR_STATUS)    { cpr_status.write(cpr_wdata) }
    is(CPR_CAUSE)     { cpr_cause.write(cpr_wdata) }
    is(CPR_EPC)       { cpr_epc := cpr_wdata }
    is(CPR_PRID)      { cpr_prid.write(cpr_wdata) }
    is(CPR_EBASE)     { cpr_ebase := cpr_wdata }
    is(CPR_CONFIG)    { cpr_config.write(cpr_wdata) }
    is(CPR_CONFIG1)   { cpr_config1.write(cpr_wdata) }
    }
  }

  io.tlbr_port.pagemask := cpr_pagemask
  io.tlbr_port.index := cpr_index
  io.tlbr_port.entry_hi := cpr_entry_hi
  io.tlbr_port.entry_lo0 := cpr_entry_lo0
  io.tlbr_port.entry_lo1 := cpr_entry_lo1

  when (io.tlbw_port.valid && !io.ex_flush.valid) {
    val wdata = io.tlbw_port.bits
    cpr_pagemask.write(wdata.pagemask.asUInt)
    cpr_entry_hi.write(wdata.entry_hi.asUInt)
    cpr_entry_lo0.write(wdata.entry_lo0.asUInt)
    cpr_entry_lo1.write(wdata.entry_lo1.asUInt)
  }

  when (io.tlbp_port.valid) {
    cpr_index.p := io.tlbp_port.bits.index.p
    when (!io.tlbp_port.bits.index.p.asBool) {
      cpr_index.index := io.tlbp_port.bits.index.index
    }
  }

  /* process exception */
  val ip = WireInit(VecInit(for (i <- 0 until 8) yield N))
  val intr_enable = !cpr_status.ERL && !cpr_status.EXL && cpr_status.IE
  val intr_valid = (cpr_cause.IP.asUInt & cpr_status.IM.asUInt).orR && intr_enable
  val intr_flush = io.ehu.valid && intr_valid
  val ex_flush = io.ehu.valid && io.ehu.ex.et =/= ET_None
  io.ehu.ip7 := cpr_cause.IP(7)
  io.ex_flush.valid := intr_flush || ex_flush
  when (io.ex_flush.valid) {
    when (cpr_status.EXL === 0.U) {
      val is_br = io.ehu.wb.is_br
      val is_ds = io.ehu.wb.is_ds
      cpr_cause.BD := is_ds
      cpr_epc := MuxCase(io.ehu.wb.pc, Array(
        is_ds -> (io.ehu.wb.pc - 4.U)))
    }
    cpr_cause.ExcCode := Mux(io.ehu.ex.et === ET_None,
      EC_Int, io.ehu.ex.code)

    when (cpr_status.ERL === 0.U) {
      cpr_status.EXL := io.ehu.ex.et =/= ET_Eret
    } .elsewhen (io.ehu.ex.et === ET_Eret) {
      cpr_status.ERL := 0.U
    }

    when (io.ehu.ex.et === ET_ADDR_ERR) {
      cpr_badvaddr := io.ehu.ex.addr
    } .elsewhen(io.ehu.ex.et === ET_TLB_Inv ||
      io.ehu.ex.et === ET_TLB_Mod ||
      io.ehu.ex.et === ET_TLB_REFILL) {
      cpr_badvaddr := io.ehu.ex.addr
      cpr_context.badvpn2 := io.ehu.ex.addr >> 13
      cpr_entry_hi.vpn := io.ehu.ex.addr >> 13
      cpr_entry_hi.asid := io.ehu.ex.asid
    }
  }
  val offset = MuxCase(0x180.U, Array(
    cpr_status.EXL -> 0x180.U,
    (io.ehu.ex.et === ET_TLB_REFILL) -> 0x000.U,
    (intr_valid && cpr_cause.IV.asBool) -> 0x200.U))
  io.ex_flush.bits.br_target := Mux(
    io.ehu.ex.et === ET_Eret, cpr_epc,
    Mux(cpr_status.BEV === 1.U, "hbfc00200".U + offset,
      "h80000000".U + offset))

  when (cpr_compare === cpr_count) { cpr_cause.IP(7) := Y }

  if (conf.log_CP0) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "CP0")
    printv(io.rport, "CP0.rport")
    printv(io.wport, "CP0.wport")
    if (conf.log_TLB) {
      printv(io.tlbr_port, "CP0.tlbr_port")
      printv(io.tlbw_port, "CP0.tlbw_port")
      printv(io.tlbp_port, "CP0.tlbp_port")
    }
    printv(io.status, "CP0.status")
    printv(io.ehu, "CP0.ehu")
    printv(io.ex_flush, "CP0.ex_flush")
  }
}
