package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class ISU(nbps:Int) extends Module {
  val io = IO(new Bundle {
    val wb = Flipped(ValidIO(new WriteBackIO))
    val idu = Flipped(DecoupledIO(new BRIDU_ISU_IO))
    val bypasses = Vec(nbps, Flipped(ValidIO(new BypassIO)))
    val alu = DecoupledIO(new ISU_ALU_IO)
    val mdu = DecoupledIO(new ISU_MDU_IO)
    val lsu = DecoupledIO(new ISU_LSU_IO)
    val bru = DecoupledIO(new ISU_BRU_IO)
    val flush = Flipped(ValidIO(new FlushIO))
  })

  val io_out_ready = io.alu.ready && io.mdu.ready && io.lsu.ready && io.bru.ready

  val fu_in = RegEnable(next=io.idu.bits, enable=io.idu.fire())
  val fu_valid = RegInit(N)
  io.idu.ready := io_out_ready || !fu_valid

  /* bypass */
  val seq_bypass_valids = for (b <- io.bypasses) yield b.valid
  val bypass_valids = Cat(seq_bypass_valids)
  val bypass_valid = bypass_valids.orR
  assert(AtMost1H(seq_bypass_valids:_*))
  val bypass_rd = Mux1H(for (i <- 0 until nbps) yield
      bypass_valids(i) -> io.bypasses(i).bits.rd_idx)
  val bypass_need_wb = Mux1H(for (i <- 0 until nbps) yield
      bypass_valids(i) -> io.bypasses(i).bits.wen)
  val bypass_data = Mux1H(for (i <- 0 until nbps) yield
      bypass_valids(i) -> io.bypasses(i).bits.data)


  /* write back */
  val wb_valid = io.wb.valid
  val wb_need_wb = io.wb.bits.wen
  val wb_data = io.wb.bits.data

  /* register set */
  val sb = Mem(32, Bool())
  val rf = Mem(32, UInt(conf.xprlen.W))

  def read_register(idx:UInt) = {
    val is_zero = idx === 0.U
    val reg_match = !sb(idx)
    val bypass_match = bypass_valid && bypass_need_wb && bypass_rd === idx
    val wb_match = wb_valid && wb_need_wb && io.wb.bits.rd_idx === idx
    val ready = is_zero || reg_match || bypass_match || wb_match
    val data = MuxCase(0.U, Array(
      (is_zero) -> 0.U,
      (reg_match) -> rf(idx),
      (bypass_match) -> bypass_data,
      (wb_match) -> wb_data))

    Cat(data, ready)
  }

  /* read registers */
  val instr = fu_in.instr.asTypeOf(new Instr)
  val op1_idx = Mux1H(Array(
    (fu_in.op1_sel === OP1_RS) -> instr.rs_idx,
    (fu_in.op1_sel === OP1_RT) -> instr.rt_idx,
    (fu_in.op1_sel === OP1_IMU) -> 0.U
  )).asUInt

  val op2_idx = Mux1H(Array(
    (fu_in.op2_sel === OP2_RS)  -> instr.rs_idx,
    (fu_in.op2_sel === OP2_RT)  -> instr.rt_idx,
    )).asUInt

  val op1_reg_status = read_register(op1_idx)
  val op2_reg_status = read_register(op2_idx)

  /* prepare operands */
  val shamt_ext = instr.shamt.asTypeOf(UInt(conf.xprlen.W))
  val se_imm = instr.imm.asTypeOf(SInt(conf.xprlen.W)).asUInt
  val ze_imm = instr.imm.asTypeOf(UInt(conf.xprlen.W))
  val ue_imm = Cat(instr.imm, 0.U((conf.xprlen - instr.imm.getWidth).W))

  val op1_data_status = Mux1H(Array(
    (fu_in.op1_sel === OP1_RS) -> op1_reg_status,
    (fu_in.op1_sel === OP1_RT) -> op1_reg_status,
    (fu_in.op1_sel === OP1_IMU) -> Cat(ue_imm, Y)
  )).asUInt

  val op2_data_status = Mux1H(Array(
    (fu_in.op2_sel === OP2_RS)  -> op2_reg_status,
    (fu_in.op2_sel === OP2_RT)  -> op2_reg_status,
    (fu_in.op2_sel === OP2_IMI) -> Cat(se_imm, Y),
    (fu_in.op2_sel === OP2_IMZ) -> Cat(ze_imm, Y),
    (fu_in.op2_sel === OP2_SA)  -> Cat(shamt_ext, Y)
  )).asUInt

  val op1_data = op1_data_status(32, 1)
  val op2_data = op2_data_status(32, 1)
  val op_ready = fu_valid && op1_data_status(0) && op2_data_status(0)

  when (io.flush.valid || (!io.idu.fire() && op_ready)) {
    fu_valid := N
  } .elsewhen(!io.flush.valid && io.idu.fire()) {
    fu_valid := Y
  }

  /* writeback */
  val rd_idx = Mux1H(Array(
    (fu_in.rd_sel === DEST_RD) -> instr.rd_idx,
    (fu_in.rd_sel === DEST_RT) -> instr.rt_idx
  )).asUInt

  when (fu_valid) { sb(rd_idx) := Y }
  when (io.wb.valid) {
    sb(io.wb.bits.rd_idx) := N
    when (io.wb.bits.wen && io.wb.bits.rd_idx =/= 0.U) {
      rf(io.wb.bits.rd_idx) := io.wb.bits.data
    }
  }

  // ALU IO
  io.alu.bits.fu_op := fu_in.fu_op
  io.alu.bits.pc := fu_in.pc
  io.alu.bits.op1 := op1_data
  io.alu.bits.op2 := op2_data
  io.alu.bits.rd_idx := rd_idx
  io.alu.valid := op_ready && fu_in.fu_type === FU_ALU

  // MDU IO
  io.mdu.bits.fu_op := fu_in.fu_op
  io.mdu.bits.pc := fu_in.pc
  io.mdu.bits.op1 := op1_data
  io.mdu.bits.op2 := op2_data
  io.mdu.bits.rd_idx := rd_idx
  io.mdu.valid := op_ready && fu_in.fu_type === FU_MDU

  // LSU IO
  io.lsu.bits.fu_op := fu_in.fu_op
  io.lsu.bits.pc := fu_in.pc
  io.lsu.bits.base := op1_data
  io.lsu.bits.offset := se_imm
  io.lsu.bits.data := op2_data
  io.lsu.bits.rd_idx := rd_idx
  io.lsu.valid := op_ready && fu_in.fu_type === FU_LSU

  // BRU IO
  io.bru.bits.fu_op := fu_in.fu_op
  io.bru.bits.pc := fu_in.pc
  io.bru.bits.rs_data := op1_data
  io.bru.bits.rt_data := op2_data
  io.bru.bits.addr := instr.addr
  io.bru.bits.se_off := se_imm
  io.bru.bits.rd_idx := rd_idx
  io.bru.valid := op_ready && fu_in.fu_type === FU_BRU

  if (conf.log_ISU) {
    printf("%d: ISU: io_out_ready=%b, fu_valid=%b, bp_valids=%b, bp_valid=%b, bp_rd=%d, bp_need_wb=%b, bp_data=%x\n", GTimer(), io_out_ready, fu_valid, bypass_valids, bypass_valid, bypass_rd, bypass_need_wb, bypass_data)
    printf("%d: ISU: wb_valid=%b, wb_need_wb=%b, wb_data=%x, sb=%b, op_ready=%x, rd_idx=%d\n", GTimer(), wb_valid, wb_need_wb, wb_data, Cat(for (i <- 0 until 32) yield sb(i)), op_ready, rd_idx)
    printf("%d: ISU: instr=%x, op1_idx=%d, op2_idx=%d, op1_reg={%b,%x}, op2_reg={%b,%x}\n", GTimer(), instr.asUInt, op1_idx, op2_idx, op1_reg_status(0), op1_reg_status(32, 1), op2_reg_status(0), op2_reg_status(32, 1))
    printf("%d: ISU: shamt_ext=%x, se_imm=%x, ze_imm=%x, ue_imm=%x, op1_data={%b,%x}, op2_data={%b,%x}\n", GTimer(), shamt_ext, se_imm, ze_imm, ue_imm, op1_data_status(0), op1_data_status(32, 1), op2_data_status(0), op2_data_status(32, 1));
    fu_in.dump("ISU.fu_in")
    io.idu.dump("ISU.idu")
    io.wb.dump("ISU.wb")
    for (i <- 0 until nbps) {
      io.bypasses(i).dump("ISU.bp."+i)
    }
    io.alu.dump("ISU.alu")
    io.mdu.dump("ISU.mdu")
    io.lsu.dump("ISU.lsu")
    io.bru.dump("ISU.bru")
    io.flush.dump("ISU.flush")
  }
}

