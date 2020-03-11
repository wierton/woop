package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._

class TLBPhyn extends Bundle {
  val pfn = UInt(24.W)
  val c   = UInt(3.W)
  val d   = Bool()
  val v   = Bool()
}

class TLBEntry extends Bundle {
  val pagemask = UInt(16.W)
  val vpn  = UInt(19.W)
  val g    = Bool()
  val asid = UInt(8.W)
  val p0 = new TLBPhyn
  val p1 = new TLBPhyn
}

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
    val ehu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val ehu_out = DecoupledIO(new PRALU_LSMDU_IO)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = ValidIO(new FlushIO)
    val wb = Flipped(ValidIO(new WriteBackIO))
    val can_log_now = Input(Bool())
  })

  /* TLB */
  val tlb_entries = Mem(conf.tlbsz, new TLBEntry)
  def tlb_entry_match(vpn:UInt, tlb_port:TLBEntry) = {
    val mask = tlb_port.pagemask.asTypeOf(UInt(32.W))
    (tlb_port.vpn & ~mask) === (vpn & ~mask) &&
    (tlb_port.g || tlb_port.asid === cpr_entry_hi.asid)
  }
  def tlb_entry_translate(vaddr:UInt, rwbit:UInt, tlb_port:TLBEntry) = {
    val mask = tlb_port.pagemask.asTypeOf(UInt(32.W))
    val eobit = (vaddr & ((mask + 1.U) << 12)).orR
    val phyn = Mux(eobit, tlb_port.p1, tlb_port.p0)
    val excode = Mux(rwbit === MX_RD, EC_TLBL, EC_TLBS)
    val res = WireInit(0.U.asTypeOf(new MMURes))
    when (phyn.v === 0.U) {
      res.ex.et := ET_TLB_Inv
      res.ex.code := excode
      res.paddr := 0.U
    } .elsewhen (phyn.d === 0.U && rwbit === MX_WR) {
      res.ex.et := ET_TLB_Mod
      res.ex.code := EC_Mod
      res.paddr := 0.U
    } .otherwise {
      val highbits = (phyn.pfn & ~mask) << 12
      val lowbits = vaddr & (((mask + 1.U) << 12) - 1.U)
      res.paddr := highbits | lowbits
      res.ex.et := ET_None
      res.ex.code := excode
    }
    res.ex.addr := vaddr
    res.ex.asid := tlb_port.asid
    res
  }
  class MMURes extends Bundle {
    val ex = new CP0Exception
    val paddr = UInt(conf.xprlen.W)
  }
  def tlb_translate(vaddr:UInt, rwbit:UInt) = {
    val tlb_rports = for (i <- 0 until conf.tlbsz) yield tlb_entries(i)
    val matches = Reverse(Cat(for (i <- 0 until conf.tlbsz) yield
      tlb_entry_match(vaddr(31, 13), tlb_rports(i))))
    val miss = !matches.orR
    val hit_res = Mux1H(for (i <- 0 until conf.tlbsz) yield
      matches(i) -> tlb_entry_translate(vaddr, rwbit, tlb_rports(i)))
    val miss_res = WireInit(0.U.asTypeOf(new MMURes))
    miss_res.ex.et := ET_TLB_REFILL
    miss_res.ex.code := Mux(rwbit === MX_RD, EC_TLBL, EC_TLBS)
    miss_res.ex.addr := vaddr
    miss_res.ex.asid := cpr_entry_hi.asid
    miss_res.paddr := 0.U
    Mux(miss, miss_res, hit_res)
  }

  def is_cached(vaddr:UInt) = vaddr(31, 29) =/= "b101".U
  def ioremap(addr:UInt) = Cat("b000".U, addr(28, 0))
  def vaddr2paddr(vaddr:UInt, rwbit:UInt) = {
    val mmu_res = tlb_translate(vaddr, rwbit)
    val vid = vaddr(31, 29)
    val paddr = Mux1H(Array(
      (vid === 4.U || vid === 5.U) -> ioremap(vaddr),
      (vid === 6.U) -> ioremap(mmu_res.paddr),
      (vid(2) === 0.U) -> Mux(cpr_status.ERL,
        vaddr, ioremap(mmu_res.paddr)),
    ))
    val et = Mux1H(Array(
      (vid === 4.U || vid === 5.U) -> ET_None,
      (vid === 6.U) -> mmu_res.ex.et,
      (vid(2) === 0.U) -> Mux(cpr_status.ERL, ET_None, mmu_res.ex.et),
    ))
    val res = WireInit(0.U.asTypeOf(new MMURes))
    res.ex.et := et
    res.ex.code := mmu_res.ex.code
    res.ex.addr := vaddr
    res.paddr := paddr
    res
  }

  /* handle memory translate request, a pipeline stage */
  val iaddr_in = RegEnable(io.iaddr.req.bits, enable=io.iaddr.req.fire())
  val iaddr_valid = RegInit(N)
  val iaddr_res = vaddr2paddr(iaddr_in.vaddr, MX_RD)
  val flush_valid = io.br_flush.valid || io.ex_flush.valid
  io.iaddr.req.ready := io.iaddr.resp.ready || !iaddr_valid
  io.iaddr.resp.valid := iaddr_valid
  io.iaddr.resp.bits.paddr := iaddr_res.paddr
  io.iaddr.resp.bits.is_cached := is_cached(iaddr_in.vaddr)
  io.iaddr.resp.bits.ex := iaddr_res.ex
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
  val lsu_op = fu_op.asTypeOf(new LSUOp)
  val lsu_vaddr = fu_in.ops.op1
  val lsu_bad_load = ((lsu_vaddr(1, 0) =/= 0.U && (fu_op === LSU_LW || fu_op === LSU_LL)) ||
    (lsu_vaddr(0) =/= 0.U && (fu_op === LSU_LH || fu_op === LSU_LHU)))
  val lsu_bad_store = ((lsu_vaddr(1, 0) =/= 0.U && (fu_op === LSU_SW || fu_op === LSU_SC)) ||
    (lsu_vaddr(0) =/= 0.U && fu_op === LSU_SH))
  val lsu_bad_addr = lsu_bad_load || lsu_bad_store
  val lsu_res = vaddr2paddr(lsu_vaddr, lsu_op.func)
  val lsu_has_ex = lsu_bad_addr || lsu_res.ex.et =/= ET_None
  val lsu_et = Mux(lsu_bad_addr, ET_ADDR_ERR, lsu_res.ex.et)
  val lsu_ec = MuxCase(lsu_res.ex.code, Array(
    lsu_bad_load  -> EC_AdEL,
    lsu_bad_store -> EC_AdES))
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }
  io.fu_out.valid := fu_valid
  io.fu_out.bits.ops := fu_in.ops
  io.fu_out.bits.paddr := lsu_res.paddr
  io.fu_out.bits.is_cached := lsu_vaddr(31, 29) =/= "b101".U
  io.fu_in.ready := io.fu_out.ready || !fu_valid

  /* cpr io */
  val is_pru = fu_valid && fu_in.ops.fu_type === FU_PRU
  val is_mfc0 = fu_valid && fu_in.ops.fu_type === FU_PRU && fu_in.ops.fu_op === PRU_MFC0
  val is_mtc0 = fu_valid && fu_in.ops.fu_type === FU_PRU && fu_in.ops.fu_op === PRU_MTC0
  val cpr_addr = Cat(fu_in.wb.instr.rd_idx, fu_in.wb.instr.sel)
  val tlbio_idx = Mux(fu_in.ops.fu_type === PRU_TLBWR, cpr_random.index, cpr_index.index)
  val tlb_cp0_port = tlb_entries(tlbio_idx)
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
  when (is_pru) {
    when (fu_in.ops.fu_op === PRU_TLBR) {
      val bnot_tlb_pagemask = ~tlb_cp0_port.pagemask.asTypeOf(UInt(32.W))
      cpr_pagemask.mask := tlb_cp0_port.pagemask
      cpr_entry_hi.vpn := tlb_cp0_port.vpn & bnot_tlb_pagemask
      cpr_entry_hi.asid := tlb_cp0_port.asid

      cpr_entry_lo0.pfn := tlb_cp0_port.p0.pfn & bnot_tlb_pagemask
      cpr_entry_lo0.c := tlb_cp0_port.p0.c
      cpr_entry_lo0.d := tlb_cp0_port.p0.d
      cpr_entry_lo0.v := tlb_cp0_port.p0.v
      cpr_entry_lo0.g := tlb_cp0_port.g

      cpr_entry_lo1.pfn := tlb_cp0_port.p1.pfn & bnot_tlb_pagemask
      cpr_entry_lo1.c := tlb_cp0_port.p1.c
      cpr_entry_lo1.d := tlb_cp0_port.p1.d
      cpr_entry_lo1.v := tlb_cp0_port.p1.v
      cpr_entry_lo1.g := tlb_cp0_port.g
    } .elsewhen(fu_in.ops.fu_op === PRU_TLBWI || fu_in.ops.fu_op === PRU_TLBWR) {
      val bnot_cpr_pagemask = ~cpr_pagemask.mask.asTypeOf(UInt(32.W))
      tlb_cp0_port.pagemask := cpr_pagemask.mask
      tlb_cp0_port.vpn := cpr_entry_hi.vpn & bnot_cpr_pagemask
      tlb_cp0_port.asid := cpr_entry_hi.asid

      tlb_cp0_port.g := cpr_entry_lo0.g & cpr_entry_lo1.g

      tlb_cp0_port.p0.pfn := cpr_entry_lo0.pfn & bnot_cpr_pagemask
      tlb_cp0_port.p0.c := cpr_entry_lo0.c
      tlb_cp0_port.p0.d := cpr_entry_lo0.d
      tlb_cp0_port.p0.v := cpr_entry_lo0.v

      tlb_cp0_port.p1.pfn := cpr_entry_lo1.pfn & bnot_cpr_pagemask
      tlb_cp0_port.p1.c := cpr_entry_lo1.c
      tlb_cp0_port.p1.d := cpr_entry_lo1.d
      tlb_cp0_port.p1.v := cpr_entry_lo1.v
    } .elsewhen (fu_in.ops.fu_op === PRU_TLBP) {
      val matches = Reverse(Cat(for (i <- 0 until conf.tlbsz) yield
        tlb_entry_match(cpr_entry_hi.vpn, tlb_entries(i))))
      val miss = !matches.orR
      cpr_index.p := miss
      when (!miss) {
        cpr_index.index := Mux1H(for (i <- 0 until conf.tlbsz) yield matches(i) -> i.U)
      }
    }

    when (fu_in.ops.fu_op === PRU_TLBWR) {
      cpr_random.index := cpr_random.index + 1.U
    }
  }
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
    is(CPR_COMPARE)   {
      cpr_compare := fu_in.ops.op1
      cpr_cause.IP(7) := N
    }
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
  io.fu_out.bits.wb := fu_in.wb
  io.fu_out.bits.wb.v := is_mfc0
  io.fu_out.bits.wb.wen := is_mfc0 && (io.fu_out.bits.ex.et === ET_None)
  io.fu_out.bits.wb.data := Mux(is_mfc0, mf_val, fu_in.wb.data)

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
  io.fu_out.bits.ex.addr := Mux(is_lsu, lsu_vaddr, fu_in.ex.addr)
  io.fu_out.bits.ex.asid := lsu_res.ex.asid

  /* process exception */
  val offset = WireInit(0.U(12.W))
  val ehu_valid = RegInit(N)
  val ehu_in = RegEnable(next=io.ehu_in.bits, enable=io.ehu_in.fire())
  val intr_enable = !cpr_status.ERL && !cpr_status.EXL && cpr_status.IE
  val intr_valid = (cpr_cause.IP.asUInt & cpr_status.IM.asUInt).orR && intr_enable
  val ex_detected = ehu_valid && (ehu_in.ex.et =/= ET_None || intr_valid)
  val ex_wb = RegNext(io.wb, init=0.U.asTypeOf(ValidIO(new WriteBackIO)))
  val ex_waiting_wb = RegInit(N)
  val ex_waiting_id = RegInit(0.U(conf.xprlen.W))
  val ex_wb_come = ex_wb.valid && ex_wb.bits.id === ex_waiting_id
  when (ex_detected) {
    ex_waiting_wb := Y
    ex_waiting_id := ehu_in.wb.id
  }
  when (ex_wb_come) { ex_waiting_wb := N }
  io.ex_flush.valid := ex_waiting_wb && ex_wb_come
  io.ehu_in.ready := (io.ehu_out.ready || !ehu_valid) &&
    !ex_detected && !ex_waiting_wb
  io.ehu_out.valid := ehu_valid
  io.ehu_out.bits := ehu_in
  io.ehu_out.bits.wb.ip7 := cpr_cause.IP(7)
  when (!io.ehu_in.fire() && io.ehu_out.fire()) {
    ehu_valid := N
  } .elsewhen (io.ehu_in.fire()) {
    ehu_valid := Y
  }
  when (cpr_compare === cpr_count) { cpr_cause.IP(7) := Y }
  when (io.ex_flush.valid && ehu_in.ex.et =/= ET_Eret) {
    when (cpr_status.EXL === 0.U) {
      when (ehu_in.ex.et === ET_None) {
        when (ehu_in.wb.is_br) {
          cpr_cause.BD := Y
          cpr_epc := ehu_in.wb.pc
        } .otherwise {
          cpr_cause.BD := N
          cpr_epc := ehu_in.wb.npc
        }
      } .otherwise {
        when (ehu_in.wb.is_ds) {
          cpr_cause.BD := Y
          cpr_epc := ehu_in.wb.pc - 4.U
        } .otherwise {
          cpr_cause.BD := N
          cpr_epc := ehu_in.wb.pc
        }
      }

      when (ehu_in.ex.et === ET_TLB_REFILL) {
        offset := 0x000.U
      } .elsewhen(intr_valid && cpr_cause.IV.asBool) {
        offset := 0x200.U
      } .otherwise {
        offset := 0x180.U
      }
    } .otherwise {
      offset := 0x180.U
    }
    cpr_cause.ExcCode := MuxCase(0.U, Array(
      (ehu_in.ex.et =/= ET_None) -> ehu_in.ex.code,
      intr_valid -> EC_Int))

    when (ehu_in.ex.et === ET_Eret) {
      when (cpr_status.ERL === 1.U) {
        cpr_status.ERL := 0.U
      } .otherwise {
        cpr_status.EXL := 0.U
      }
    } .otherwise {
      cpr_status.EXL := 1.U
    }

    when (ehu_in.ex.et === ET_ADDR_ERR) {
      cpr_badvaddr := ehu_in.ex.addr
    } .elsewhen(ehu_in.ex.et === ET_TLB_Inv ||
      ehu_in.ex.et === ET_TLB_Mod ||
      ehu_in.ex.et === ET_TLB_REFILL) {
      val vaddr = ehu_in.ex.addr
      cpr_badvaddr := vaddr
      cpr_context.badvpn2 := vaddr >> 13
      cpr_entry_hi.vpn := vaddr >> 13
      cpr_entry_hi.asid := ehu_in.ex.asid
    }
  }
  when (io.ex_flush.valid && ehu_in.ex.et === ET_Eret) {
    when (cpr_status.ERL) {
      cpr_status.ERL := N
    } .otherwise {
      cpr_status.EXL := N
    }
  }
  io.ex_flush.bits.br_target := Mux(
    ehu_in.ex.et === ET_Eret, cpr_epc,
    Mux(cpr_status.BEV === 1.U, "hbfc00200".U + offset,
      "h80000000".U + offset))

  if (conf.log_PRU) {
    when (io.can_log_now) { dump() }
  }
  def dump():Unit = {
    printf("%d: PRU: fu_valid=%b, is_mfc0=%b, is_mtc0=%b, cpr_addr=%b, mf_val=%x, can_update_ex=%b, offset=%x, intr_enable=%b, intr_valid=%b\n", GTimer(), fu_valid, is_mfc0, is_mtc0, cpr_addr, mf_val, can_update_ex, offset, intr_enable, intr_valid)
    printf("%d: PRU: is_pru=%b, ex_detected=%b, ex_wait_wb=%b, ex_wait_id=%x, ex_wb_come=%b\n", GTimer(), is_pru, ex_detected, ex_waiting_wb, ex_waiting_id, ex_wb_come)
    printf("%d: PRU: is_lsu=%b, lsu_vaddr=%x, lsu_bad_load=%b, lsu_bad_store=%b, lsu_bad_addr=%x, lsu_res={%x, et:%d, code:%d}, lsu_has_ex=%b, lsu_et=%d, lsu_ec=%d\n", GTimer(), is_lsu, lsu_vaddr, lsu_bad_load, lsu_bad_store, lsu_bad_addr, lsu_res.paddr, lsu_res.ex.et, lsu_res.ex.code, lsu_has_ex, lsu_et, lsu_ec)
    printf("%d: PRU: entry_lo0=%x, entry_lo1=%x, context=%x, pagemask=%x, wired=%x, badvaddr=%x\n", GTimer(), cpr_entry_lo0.asUInt, cpr_entry_lo1.asUInt, cpr_context.asUInt, cpr_pagemask.asUInt, cpr_wired.asUInt, cpr_badvaddr.asUInt)
    printf("%d: PRU: count=%x, entry_hi=%x, compare=%x, status=%x, cause=%x, epc=%x, prid=%x\n", GTimer(), cpr_count.asUInt, cpr_entry_hi.asUInt, cpr_compare.asUInt, cpr_status.asUInt, cpr_cause.asUInt, cpr_epc.asUInt, cpr_prid.asUInt)
    printf("%d: PRU: ebase=%x, config=%x, config1=%x\n", GTimer(), cpr_ebase.asUInt, cpr_config.asUInt, cpr_config1.asUInt)
    io.fu_in.dump("PRU.fu_in")
    io.fu_out.dump("PRU.fu_out")
    io.ehu_in.dump("PRU.ehu_in")
    io.ehu_out.dump("PRU.ehu_out")
    io.ex_flush.dump("PRU.exflush")
  }
}
