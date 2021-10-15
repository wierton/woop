package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._


class EXU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new ISU_EXU_IO))
    val fu_out = DecoupledIO(new EXU_EHU_IO)

    val cp0_rport = new CPR_RPORT
    val cp0_wport = ValidIO(new CPR_WPORT)
    val cp0_tlbr_port = new CP0_TLBR_PORT
    val cp0_tlbw_port = ValidIO(new CP0_TLBW_PORT)
    val cp0_tlbp_port = ValidIO(new CP0_TLBP_PORT)

    val daddr = new TLBTransaction
    val tlb_rport = new TLB_RPORT
    val tlb_wport = ValidIO(new TLB_WPORT)
    val tlb_pport = new TLB_PPORT

    val icache_control = ValidIO(new CacheControl)

    val wb = Flipped(ValidIO(new WriteBackIO))
    val bp = ValidIO(new BypassIO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
    val enable_bug = Input(Bool())
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire(), init=0.U.asTypeOf(io.fu_in.bits))
  val fu_valid = RegInit(N)

  io.fu_in.ready := (io.fu_out.ready || !fu_valid) && !io.ex_flush.valid

  val fu_type = fu_in.ops.fu_type
  val fu_op   = fu_in.ops.fu_op
  val op1     = fu_in.ops.op1
  val op2     = fu_in.ops.op2
  val op2_sa  = op2(REG_SZ - 1, 0).asUInt

  def ins(op1:UInt, op2:UInt, lsb:UInt, msb:UInt) = {
    val len = (msb - lsb) + 1.U(6.W)
    val mask = (1.U << len) - 1.U
    val res = (op2 & ~(mask << lsb)) | ((op1 & mask) << lsb)
    res(31, 0)
  }

  def ext(op1:UInt, op2:UInt, lsb:UInt, msb:UInt) = {
    val size = msb + 1.U(6.W)
    val mask = (1.U << size) - 1.U
    val res = ((op1 & (mask << lsb)) >> lsb)
    res(31, 0)
  }

  val alu_wdata = Mux1H(Array(
    (fu_op === ALU_ADD)  -> (op1 + op2).asUInt,
    (fu_op === ALU_SUB)  -> (op1 - op2).asUInt,
    (fu_op === ALU_SLL)  -> (op1 << op2_sa).asUInt,
    (fu_op === ALU_SRL)  -> (op1 >> op2_sa).asUInt,
    (fu_op === ALU_SRA)  -> (op1.asSInt >> op2_sa).asUInt,
    (fu_op === ALU_AND)  -> (op1 & op2).asUInt,
    (fu_op === ALU_OR)   -> (op1 | op2).asUInt,
    (fu_op === ALU_XOR)  -> (op1 ^ op2).asUInt,
    (fu_op === ALU_NOR)  -> ~(op1 | op2).asUInt,
    (fu_op === ALU_SLT)  -> (op1.asSInt < op2.asSInt).asUInt,
    (fu_op === ALU_SLTU) -> (op1 < op2).asUInt,
    (fu_op === ALU_LUI)  -> op2.asUInt,
    (fu_op === ALU_MOVN) -> op1.asUInt,
    (fu_op === ALU_MOVZ) -> op1.asUInt,
    (fu_op === ALU_ADD_OV) -> (op1 + op2).asUInt,
    (fu_op === ALU_SUB_OV) -> (op1 - op2).asUInt,
    (fu_op === ALU_CLZ)  -> CLZ_32(op1),
    (fu_op === ALU_CLO)  -> CLO_32(op1),
    (fu_op === ALU_SEB)  -> op1(7, 0).asTypeOf(SInt(32.W)).asUInt,
    (fu_op === ALU_SEH)  -> op1(15, 0).asTypeOf(SInt(32.W)).asUInt,
    (fu_op === ALU_WSBH) -> Cat(op1(23, 16), op1(31, 24), op1(7, 0), op1(15, 8)),
    (fu_op === ALU_INS)  -> ins(op1, op2, fu_in.wb.instr.lsb, fu_in.wb.instr.msb),
    (fu_op === ALU_EXT)  -> ext(op1, op2, fu_in.wb.instr.lsb, fu_in.wb.instr.msb),
    (fu_op === ALU_ROTR) -> ((op1 >> op2_sa) | (op1 << (32.U - op2_sa))),
  ))(31, 0)
  val alu_ov = Mux1H(Array(
    (fu_op === ALU_ADD_OV)  -> ((!op1(31) && !op2(31) && alu_wdata(31)) || (op1(31) && op2(31) && !alu_wdata(31))),
    (fu_op === ALU_SUB_OV)  -> ((!op1(31) && op2(31) && alu_wdata(31)) || (op1(31) && !op2(31) && !alu_wdata(31)))))
  val alu_wen = MuxCase(!alu_ov, Array(
    (fu_op === ALU_MOVN) -> (op2 =/= 0.U),
    (fu_op === ALU_MOVZ) -> (op2 === 0.U)))
  val alu_ex = 0.U.asTypeOf(new CP0Exception)
  alu_ex.et := Mux(alu_ov, ET_Ov, ET_None)
  alu_ex.code := EC_Ov

  /* lsu addr translation */
  val lsu_op = io.fu_in.bits.ops.fu_op.asTypeOf(new LSUOp)
  val lsu_ex = io.daddr.resp.bits.ex
  io.daddr.req.valid := io.fu_in.fire() &&
    io.fu_in.bits.ops.fu_type === FU_LSU
  io.daddr.req.bits.vaddr := io.fu_in.bits.ops.op1
  io.daddr.req.bits.func := lsu_op.func
  io.daddr.req.bits.len := lsu_op.len
  io.daddr.req.bits.is_aligned := lsu_op.align
  io.daddr.resp.ready := Y

  val cpr_addr = Cat(fu_in.wb.instr.rd_idx, fu_in.wb.instr.sel)

  /* mfc0 */
  io.cp0_rport.addr := cpr_addr
  val pru_wen = fu_type === FU_PRU && fu_op === PRU_MFC0
  val pru_wdata = io.cp0_rport.data

  /* mtc0 */
  io.cp0_wport.valid := io.fu_out.fire() &&
    fu_type === FU_PRU && fu_op === PRU_MTC0
  io.cp0_wport.bits.addr := cpr_addr
  io.cp0_wport.bits.data := op1

  /* pru */
  val pru_ex = WireInit(0.U.asTypeOf(new CP0Exception))
  val pru_trap = MuxLookup(fu_op, 0.U, Array(
    PRU_TGE   -> Cat(Y, op1.asSInt >= op2.asSInt),
    PRU_TGEU  -> Cat(Y, op1 >= op2),
    PRU_TLT   -> Cat(Y, op1.asSInt < op2.asSInt),
    PRU_TLTU  -> Cat(Y, op1 < op2),
    PRU_TEQ   -> Cat(Y, op1 === op2),
    PRU_TNE   -> Cat(Y, op1 =/= op2),
    PRU_TGEI  -> Cat(Y, op1.asSInt >= op2.asSInt),
    PRU_TGEIU -> Cat(Y, op1 >= op2),
    PRU_TLTI  -> Cat(Y, op1.asSInt < op2.asSInt),
    PRU_TLTIU -> Cat(Y, op1 < op2),
    PRU_TEQI  -> Cat(Y, op1 === op2),
    PRU_TNEI  -> Cat(Y, op1 =/= op2),
  ))
  val pru_normal_et = MuxLookup(fu_op, fu_in.ex.et, Array(
    PRU_SYSCALL -> ET_Sys,
    PRU_BREAK   -> ET_Bp,
    PRU_ERET    -> ET_Eret))
  val pru_normal_code = MuxLookup(fu_op, fu_in.ex.code, Array(
    PRU_SYSCALL -> EC_Sys,
    PRU_BREAK   -> EC_Bp))
  pru_ex := fu_in.ex
  pru_ex.et := Mux(pru_trap(1), Mux(pru_trap(0), ET_Tr,
    ET_None), pru_normal_et)
  pru_ex.code := Mux(pru_trap(1), Mux(pru_trap(0), EC_Tr,
    0.U), pru_normal_code)

  /* exception */
  io.fu_out.bits.ex := MuxLookup(fu_type, fu_in.ex, Array(
    FU_ALU -> alu_ex, FU_LSU -> lsu_ex, FU_PRU -> pru_ex))

  /* fu_out */
  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb := fu_in.wb
  val wb_info = MuxLookup(fu_type,
    Cat(fu_in.wb.v, fu_in.wb.wen, fu_in.wb.data), Array(
    FU_ALU -> Cat(Y, alu_wen, alu_wdata),
    FU_PRU -> Cat(pru_wen, pru_wen, pru_wdata)))
  io.fu_out.bits.wb.v := wb_info(33)
  io.fu_out.bits.wb.wen := wb_info(32)
  io.fu_out.bits.wb.data := wb_info(31, 0)
  io.fu_out.bits.ops := fu_in.ops
  io.fu_out.bits.ops.op1 := Mux(fu_in.ops.fu_type === FU_LSU, 
    io.daddr.resp.bits.paddr, fu_in.ops.op1)
  io.fu_out.bits.ops.fu_type := Mux(fu_in.ops.fu_type === FU_LSU, Mux(
    lsu_ex.et =/= ET_None, FU_PRU, FU_LSU), fu_in.ops.fu_type)
  io.fu_out.bits.is_cached := Y

  /* write back */
  io.bp.valid := io.fu_out.valid
  io.bp.bits.v := io.fu_out.bits.wb.v
  io.bp.bits.rd_idx := io.fu_out.bits.wb.rd_idx
  io.bp.bits.wen := io.fu_out.bits.wb.wen
  io.bp.bits.data := io.fu_out.bits.wb.data

  /* icache_control */
  val cache_control_target = fu_in.ops.op2(1, 0)
  val cache_control_op = fu_in.ops.op2(4, 2)
  io.icache_control.valid := io.fu_out.fire() &&
    fu_type === FU_PRU && fu_op === PRU_CACHE &&
    cache_control_target === CONTROL_ICACHE
  io.icache_control.bits.op := cache_control_op
  io.icache_control.bits.addr := fu_in.ops.op1

  /* tlbr */
  val tlbr_mask_correct = io.tlb_rport.entry.pagemask.asTypeOf(UInt(32.W))
  val tlbr_mask = Mux(io.enable_bug, ~tlbr_mask_correct, tlbr_mask_correct)
  io.tlb_rport.index := io.cp0_tlbr_port.index.index
  io.cp0_tlbw_port.valid := io.fu_out.fire() &&
    fu_type === FU_PRU && fu_op === PRU_TLBR
  io.cp0_tlbw_port.bits.pagemask._0 := 0.U
  io.cp0_tlbw_port.bits.pagemask._1 := 0.U
  io.cp0_tlbw_port.bits.pagemask.mask := tlbr_mask
  io.cp0_tlbw_port.bits.entry_hi.vpn := io.tlb_rport.entry.vpn & ~tlbr_mask
  io.cp0_tlbw_port.bits.entry_hi.asid := io.tlb_rport.entry.asid & ~tlbr_mask
  io.cp0_tlbw_port.bits.entry_hi._0 := 0.U
  io.cp0_tlbw_port.bits.entry_lo0.v := io.tlb_rport.entry.p0.v
  io.cp0_tlbw_port.bits.entry_lo0.c := io.tlb_rport.entry.p0.c
  io.cp0_tlbw_port.bits.entry_lo0.d := io.tlb_rport.entry.p0.d
  io.cp0_tlbw_port.bits.entry_lo0.g := io.tlb_rport.entry.g
  io.cp0_tlbw_port.bits.entry_lo0.pfn := io.tlb_rport.entry.p0.pfn & ~tlbr_mask
  io.cp0_tlbw_port.bits.entry_lo0._0 := 0.U
  io.cp0_tlbw_port.bits.entry_lo0._1 := 0.U
  io.cp0_tlbw_port.bits.entry_lo1.v := io.tlb_rport.entry.p1.v
  io.cp0_tlbw_port.bits.entry_lo1.c := io.tlb_rport.entry.p1.c
  io.cp0_tlbw_port.bits.entry_lo1.d := io.tlb_rport.entry.p1.d
  io.cp0_tlbw_port.bits.entry_lo1.g := io.tlb_rport.entry.g
  io.cp0_tlbw_port.bits.entry_lo1.pfn := io.tlb_rport.entry.p1.pfn & ~tlbr_mask
  io.cp0_tlbw_port.bits.entry_lo1._0 := 0.U
  io.cp0_tlbw_port.bits.entry_lo1._1 := 0.U

  /* tlbw */
  val cpr_random = RegInit(0.U.asTypeOf(new CP0Random))
  val tlbw_mask = ~io.cp0_tlbr_port.pagemask.mask.asTypeOf(UInt(32.W))
  io.tlb_wport.valid := io.fu_out.fire() &&
    fu_type === FU_PRU && (fu_op === PRU_TLBWI ||
    fu_op === PRU_TLBWR)
  io.tlb_wport.bits.index := Mux(fu_op === PRU_TLBWI, io.cp0_tlbr_port.index.index, cpr_random.index)
  io.tlb_wport.bits.entry.pagemask := io.cp0_tlbr_port.pagemask.mask
  io.tlb_wport.bits.entry.vpn := io.cp0_tlbr_port.entry_hi.vpn & tlbw_mask
  io.tlb_wport.bits.entry.asid := io.cp0_tlbr_port.entry_hi.asid
  io.tlb_wport.bits.entry.g := io.cp0_tlbr_port.entry_lo0.g && io.cp0_tlbr_port.entry_lo1.g
  io.tlb_wport.bits.entry.p0.pfn := io.cp0_tlbr_port.entry_lo0.pfn & tlbw_mask
  io.tlb_wport.bits.entry.p0.c := io.cp0_tlbr_port.entry_lo0.c
  io.tlb_wport.bits.entry.p0.d := io.cp0_tlbr_port.entry_lo0.d
  io.tlb_wport.bits.entry.p0.v := io.cp0_tlbr_port.entry_lo0.v
  io.tlb_wport.bits.entry.p1.pfn := io.cp0_tlbr_port.entry_lo1.pfn & tlbw_mask
  io.tlb_wport.bits.entry.p1.c := io.cp0_tlbr_port.entry_lo1.c
  io.tlb_wport.bits.entry.p1.d := io.cp0_tlbr_port.entry_lo1.d
  io.tlb_wport.bits.entry.p1.v := io.cp0_tlbr_port.entry_lo1.v
  when (io.tlb_wport.valid && fu_op === PRU_TLBWR && !io.ex_flush.valid) {
    cpr_random.index := cpr_random.index + 1.U
  }

  /* tlbp */
  io.tlb_pport.entry_hi := io.cp0_tlbr_port.entry_hi
  io.cp0_tlbp_port.valid := io.fu_out.fire() &&
    fu_type === FU_PRU && fu_op === PRU_TLBP
  io.cp0_tlbp_port.bits.index := io.tlb_pport.index

  /* pipeline logic */
  when (io.ex_flush.valid || (!io.fu_in.fire() && io.fu_out.fire())) {
    fu_valid := N
  } .elsewhen(!io.ex_flush.valid && io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_EXU) {
    when (io.can_log_now) { dump() }
  }

  def dump() = {
    printv(this, "EXU")
    printv(io.fu_in, "EXU.fu_in")
    printv(io.fu_out, "EXU.fu_out")

    printv(io.cp0_rport, "EXU.cp0_rport")
    printv(io.cp0_wport, "EXU.cp0_wport")

    if (conf.log_TLB) {
      printv(io.cp0_tlbr_port, "EXU.cp0_tlbr_port")
      printv(io.cp0_tlbw_port, "EXU.cp0_tlbw_port")
      printv(io.cp0_tlbp_port, "EXU.cp0_tlbp_port")

      printv(io.daddr, "EXU.daddr")
      printv(io.tlb_rport, "EXU.tlb_rport")
      printv(io.tlb_wport, "EXU.tlb_wport")
      printv(io.tlb_pport, "EXU.tlb_pport")
    }

    printv(io.ex_flush, "EXU.ex_flush")
    printv(io.wb, "EXU.wb")
    printv(io.bp, "EXU.bp")
  }
}
