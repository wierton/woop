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
  def cpr_random    = Reg(new CP0Random)
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
    cpr_random.init()
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

class PRU extends CPRS with LSUConsts {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val fu_in = Flipped(DecoupledIO(new PRALU_FU_IO))
    val fu_out = DecoupledIO(new PRU_OUT_PRALU)
    val exinfo = Flipped(ValidIO(new CP0ExInfo))
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

  /* lsu addr translation */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_op = fu_in.ops.fu_op
  val is_lsu = fu_valid && fu_in.ops.fu_type === FU_LSU
  val lsu_vaddr = fu_in.ops.op1
  val lsu_bad_load = ((lsu_vaddr(1, 0) =/= 0.U && (fu_op === LSU_LW || fu_op === LSU_LL)) ||
    (lsu_vaddr(0) =/= 0.U && (fu_op === LSU_LH || fu_op === LSU_LHU)))
  val lsu_bad_store = ((lsu_vaddr(1, 0) =/= 0.U && (fu_op === LSU_SW || fu_op === LSU_SC)) ||
    (lsu_vaddr(0) =/= 0.U && fu_op === LSU_SH))
  val lsu_has_ex = lsu_bad_load || lsu_bad_store
  val lsu_et = ET_ADDR_ERR
  val lsu_ec = Mux1H(Array(
    lsu_bad_load  -> EC_AdEL,
    lsu_bad_store -> EC_AdES))
  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }
  io.fu_out.valid := fu_valid
  io.fu_out.bits.ops := fu_in.ops
  io.fu_out.bits.paddr := naive_tlb_translate(lsu_vaddr)
  io.fu_out.bits.addr := lsu_vaddr
  io.fu_out.bits.is_cached := lsu_vaddr(31, 29) =/= "b101".U
  io.fu_in.ready := io.fu_out.ready || !fu_valid

  /* cpr io */
  val is_pru = fu_valid && fu_in.ops.fu_type === FU_PRU
  val is_mfc0 = fu_valid && fu_in.ops.fu_type === FU_PRU && fu_in.ops.fu_op === PRU_MFC0
  val is_mtc0 = fu_valid && fu_in.ops.fu_type === FU_PRU && fu_in.ops.fu_op === PRU_MTC0
  val cpr_addr = Cat(fu_in.wb.instr.rd_idx, fu_in.wb.instr.sel)
  val mf_val = Mux1H(Array(
    (cpr_addr === CPR_INDEX)     -> cpr_index.asUInt,
    (cpr_addr === CPR_RANDOM)    -> cpr_random.asUInt,
    (cpr_addr === CPR_ENTRY_LO0) -> cpr_entry_lo0.asUInt,
    (cpr_addr === CPR_ENTRY_LO1) -> cpr_entry_lo1.asUInt,
    (cpr_addr === CPR_CONTEXT)   -> cpr_context.asUInt,
    (cpr_addr === CPR_PAGEMASK)  -> cpr_pagemask.asUInt,
    (cpr_addr === CPR_WIRED)     -> cpr_wired.asUInt,
    (cpr_addr === CPR_BAD_VADDR) -> cpr_badvaddr.asUInt,
    (cpr_addr === CPR_COUNT)     -> cpr_count.asUInt,
    (cpr_addr === CPR_ENTRY_HI)  -> cpr_entry_hi.asUInt,
    (cpr_addr === CPR_COMPARE)   -> cpr_compare.asUInt,
    (cpr_addr === CPR_STATUS)    -> cpr_status.asUInt,
    (cpr_addr === CPR_CAUSE)     -> cpr_cause.asUInt,
    (cpr_addr === CPR_EPC)       -> cpr_epc.asUInt,
    (cpr_addr === CPR_PRID)      -> cpr_prid.asUInt,
    (cpr_addr === CPR_EBASE)     -> cpr_ebase.asUInt,
    (cpr_addr === CPR_CONFIG)    -> cpr_config.asUInt,
    (cpr_addr === CPR_CONFIG1)   -> cpr_config1.asUInt,
  ))
  when (is_mtc0) {
    switch (cpr_addr) {
    is(CPR_INDEX)     { cpr_index.write(fu_in.ops.op1) }
    is(CPR_RANDOM)    { cpr_random.write(fu_in.ops.op1) }
    is(CPR_ENTRY_LO0) { cpr_entry_lo0.write(fu_in.ops.op1) }
    is(CPR_ENTRY_LO1) { cpr_entry_lo1.write(fu_in.ops.op1) }
    is(CPR_CONTEXT)   { cpr_context.write(fu_in.ops.op1) }
    is(CPR_PAGEMASK)  { cpr_pagemask.write(fu_in.ops.op1) }
    is(CPR_WIRED)     { cpr_wired.write(fu_in.ops.op1) }
    is(CPR_BAD_VADDR) { cpr_badvaddr := fu_in.ops.op1 }
    is(CPR_COUNT)     { }
    is(CPR_ENTRY_HI)  { cpr_entry_hi.write(fu_in.ops.op1) }
    is(CPR_COMPARE)   { cpr_compare := fu_in.ops.op1 }
    is(CPR_STATUS)    { cpr_status.write(fu_in.ops.op1) }
    is(CPR_CAUSE)     { cpr_cause.write(fu_in.ops.op1) }
    is(CPR_EPC)       { cpr_epc := fu_in.ops.op1 }
    is(CPR_PRID)      { cpr_prid.write(fu_in.ops.op1) }
    is(CPR_EBASE)     { cpr_ebase := fu_in.ops.op1 }
    is(CPR_CONFIG)    { cpr_config.write(fu_in.ops.op1) }
    is(CPR_CONFIG1)   { cpr_config1.write(fu_in.ops.op1) }
    }
  }

  /* write back */
  io.fu_out.bits.wb.v := fu_in.wb.v || is_mfc0 || is_lsu
  io.fu_out.bits.wb.id := fu_in.wb.id
  io.fu_out.bits.wb.pc := fu_in.wb.pc
  io.fu_out.bits.wb.instr := fu_in.wb.instr
  io.fu_out.bits.wb.rd_idx := fu_in.wb.rd_idx
  io.fu_out.bits.wb.wen := (fu_in.wb.wen || is_mfc0) &&
    (io.fu_out.bits.ex.et === ET_None)
  io.fu_out.bits.wb.data := Mux(is_mfc0, mf_val, fu_in.wb.data)
  io.fu_out.bits.wb.is_ds := fu_in.wb.is_ds

  /* c0 instruction exception */
  val can_update_ex = (is_pru || is_lsu) && fu_in.ex.et === ET_None
  io.fu_out.bits.ex.et := Mux(can_update_ex,
    Mux1H(Array(
      (is_lsu && lsu_has_ex) -> lsu_et,
      (is_pru && fu_op === PRU_SYSCALL)-> ET_Sys,
      (is_pru && fu_op === PRU_BREAK)  -> ET_Bp,
      (is_pru && fu_op === PRU_ERET)   -> ET_Eret)),
    fu_in.ex.et)
  io.fu_out.bits.ex.code := Mux(can_update_ex,
    Mux1H(Array(
      (is_lsu && lsu_has_ex) -> lsu_ec,
      (is_pru && fu_op === PRU_SYSCALL)-> EC_Sys,
      (is_pru && fu_op === PRU_BREAK)  -> EC_Bp)),
    fu_in.ex.code)

  /* process exception */
  val offset = WireInit(0.U(12.W))
  io.ex_flush.valid := io.exinfo.valid &&
    io.exinfo.bits.ex.et =/= ET_None
  when (io.ex_flush.valid) {
    when (cpr_status.EXL === 0.U) {
      when (io.exinfo.bits.is_ds) {
        cpr_cause.BD := Y
        cpr_epc := io.exinfo.bits.pc - 4.U
      } .otherwise {
        cpr_cause.BD := N
        cpr_epc := io.exinfo.bits.pc
      }

      when (io.exinfo.bits.ex.et === ET_TLB_REFILL) {
        offset := 0x000.U
      } .elsewhen(io.exinfo.bits.ex.et === ET_Int && cpr_cause.IV.asBool) {
        offset := 0x200.U
      } .otherwise {
        offset := 0x180.U
      }
    } .otherwise {
      offset := 0x180.U
    }
    cpr_cause.ExcCode := io.exinfo.bits.ex.code

    when (io.exinfo.bits.ex.et === ET_Eret) {
      when (cpr_status.ERL === 1.U) {
        cpr_status.ERL := 0.U
      } .otherwise {
        cpr_status.EXL := 0.U
      }
    } .otherwise {
      cpr_status.EXL := 1.U
    }

    when (io.exinfo.bits.ex.et === ET_ADDR_ERR) {
      cpr_badvaddr := io.exinfo.bits.addr
    }
  }
  io.ex_flush.bits.br_target := Mux(
    io.exinfo.bits.ex.et === ET_Eret, cpr_epc,
    Mux(cpr_status.BEV === 1.U, "hbfc00200".U + offset,
      "h80000000".U + offset))

  if (conf.log_PRU) {
    when (TraceTrigger()) { dump() }
  }
  def dump():Unit = {
    printf("%d: PRU: fu_valid=%b, is_mfc0=%b, is_mtc0=%b, cpr_addr=%b, mf_val=%x, can_update_ex=%b, offset=%x\n", GTimer(), fu_valid, is_mfc0, is_mtc0, cpr_addr, mf_val, can_update_ex, offset)
    printf("%d: PRU: entry_lo0=%x, entry_lo1=%x, context=%x, pagemask=%x, wired=%x, badvaddr=%x\n", GTimer(), cpr_entry_lo0.asUInt, cpr_entry_lo1.asUInt, cpr_context.asUInt, cpr_pagemask.asUInt, cpr_wired.asUInt, cpr_badvaddr.asUInt)
    printf("%d: PRU: count=%x, entry_hi=%x, compare=%x, status=%x, cause=%x, epc=%x, prid=%x\n", GTimer(), cpr_count.asUInt, cpr_entry_hi.asUInt, cpr_compare.asUInt, cpr_status.asUInt, cpr_cause.asUInt, cpr_epc.asUInt, cpr_prid.asUInt)
    printf("%d: PRU: ebase=%x, config=%x, config1=%x\n", GTimer(), cpr_ebase.asUInt, cpr_config.asUInt, cpr_config1.asUInt)
    io.fu_in.dump("PRU.fu_in")
    io.fu_out.dump("PRU.fu_out")
    io.exinfo.dump("PRU.exinfo")
    io.ex_flush.dump("PRU.exflush")
  }
}
