package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._


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
    val tlbr_port = Flipped(new TLBR_PORT)
    val tlbw_port = Flipped(ValidIO(new TLBW_PORT))
    val tlbp_port = Flipped(ValidIO(new TLBP_PORT))
    val exu = Flipped(new EXU_CP0_IO)
    val can_log_now = Input(Bool())
  })

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
  when (io.wport.valid) {
    switch (io.wport.addr) {
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
  io.tlbr_port.entry_hi := cpr_entry_hi
  io.tlbr_port.entry_lo0 := cpr_entry_lo0
  io.tlbr_port.entry_lo1 := cpr_entry_lo

  when (io.tlbw_port.valid) {
    val wdata = io.tlbw_port.bits
    cpr_pagemask.write(wdata.pagemask.asUInt)
    cpr_entry_hi.write(wdata.entry_hi.asUInt)
    cpr_entry_lo0.write(wdata.entry_lo0.asUInt)
    cpr_entry_lo1.write(wdata.entry_lo1.asUInt)
  }

  when (io.tlbp_port.valid) {
    cpr_index.p := io.tlbp_port.index.p
    when (io.tlbp_port.index.p) {
      cpr_index.index := io.tlbp_port.index.index
    }
  }

  /* process exception */
  val ip = WireInit(VecInit(8, N)))
  val is_mtc0_cause = io.cp0_wport.valid && io.cp0_wport.addr === CPR_CAUSE
  val cpr_wcause = io.cp0_wport.data.asTypeOf(new CP0Cause)
  val intr_enable = !cpr_status.ERL && !cpr_status.EXL && cpr_status.IE
  val intr_valid = (ip.asUInt & cpr_status.IM.asUInt).orR
    && intr_enable && io.exu.ex.et === ET_None
  val offset = WireInit(0.U(12.W))
  ip(0) := is_mtc0_cause && cpr_wcause.IP(0)
  ip(1) := is_mtc0_cause && cpr_wcause.IP(1)
  ip(7) := cpr_cause.IP(7)
  io.ex_flush.valid := io.exu.valid && (
    io.exu.ex.et =/= ET_None || intr_valid)
  when (io.ex_flush.valid) {
    when (cpr_status.EXL === 0.U) {
      val is_br = io.exu.wb.is_br
      val is_ds = io.exu.wb.is_ds
      cpr_cause.BD := (intr_valid && is_br) || is_ds
      cpr_epc := MuxCase(io.exu.wb.pc, Array(
        (intr_valid && is_ds) -> io.exu.wb.npc,
        (!intr_valid && is_ds) -> io.exu.wb.pc - 4.U))
    }
    cpr_cause.ExcCode := MuxCase(io.exu.ex.et === ET_None,
      EC_Int, io.exu.ex.code)

    when (cpr_status.ERL === 0.U) {
      cpr_status.EXL := io.exu.ex.et === ET_Eret
    } .elsewhen (io.exu.ex.et === ET_Eret) {
        cpr_status.ERL := 0.U
    }

    when (io.exu.ex.et === ET_ADDR_ERR) {
      cpr_badvaddr := io.exu.ex.addr
    } .elsewhen(io.exu.ex.et === ET_TLB_Inv ||
      io.exu.ex.et === ET_TLB_Mod ||
      io.exu.ex.et === ET_TLB_REFILL) {
      cpr_badvaddr := io.exu.ex.addr
      cpr_context.badvpn2 := io.exu.ex.addr >> 13
      cpr_entry_hi.vpn := io.exu.ex.addr >> 13
      cpr_entry_hi.asid := io.exu.ex.asid
    }
  }
  val offset = MuxCase(0x180.U, Array(
    cpr_status.EXL -> 0x180.U
    (io.exu.ex.et === ET_TLB_REFILL) -> 0x000.U,
    (intr_valid && cpr_cause.IV.asBool) -> 0x200.U))
  io.ex_flush.bits.br_target := Mux(
    io.exu.ex.et === ET_Eret, cpr_epc,
    Mux(cpr_status.BEV === 1.U, "hbfc00200".U + offset,
      "h80000000".U + offset))

  when (cpr_compare === cpr_count) { cpr_cause.IP(7) := Y }
  io.exu.intr := (for(i <- 2 until 8) yield
    cpr_cause.IP(i) & cpr_status.IM(i)).orR && intr_enable
}
