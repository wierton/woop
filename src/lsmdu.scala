package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

import woop.utils._


class LSMDUPipelineStage extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new Bundle {
      val wb = new WriteBackIO
      val fu_type = Output(UInt(FU_TYPE_SZ.W))
    }))
    val fu_out = ValidIO(new WriteBackIO)
    val can_log_now = Input(Bool())
  })

  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  val fu_valid = RegInit(N)
  io.fu_in.ready := Y
  io.fu_out.valid := fu_valid
  io.fu_out.bits := fu_in.wb
  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_LSMDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("%d: LSMDU.psu.io.fu_in: fu_valid=%b, io.fu_in[%b,%b]={fu_type=%d}\n", GTimer(), fu_valid, io.fu_in.valid, io.fu_in.ready, io.fu_in.bits.fu_type)
    printv(io.fu_out, "LSMD-PSU.fu_out")
  }
}

class LSMDU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new EHU_LSMDU_IO))
    val wb = ValidIO(new WriteBackIO)
    val divider = new DividerIO
    val multiplier = new MultiplierIO
    val dmem = new MemIO
    val can_log_now = Input(Bool())
  })

  val lsu = Module(new LSU)
  val mdu = Module(new MDU)
  val psu = Module(new LSMDUPipelineStage)

  /* LSU IO */
  val to_lsu = !mdu.io.working &&
    io.fu_in.bits.ops.fu_type === FU_LSU
  lsu.io.fu_in.valid := io.fu_in.valid && to_lsu
  lsu.io.fu_in.bits := io.fu_in.bits
  lsu.io.dmem <> io.dmem
  lsu.io.can_log_now := io.can_log_now

  /* MDU IO */
  val to_mdu = !lsu.io.working &&
    io.fu_in.bits.ops.fu_type === FU_MDU
  mdu.io.fu_in.valid := io.fu_in.valid && to_mdu
  mdu.io.fu_in.bits := io.fu_in.bits
  mdu.io.divider <> io.divider
  mdu.io.multiplier <> io.multiplier
  mdu.io.can_log_now := io.can_log_now

  /* pipeline stage for ALU,BRU,PRU */
  val to_psu = !lsu.io.working && !mdu.io.working &&
    (io.fu_in.bits.ops.fu_type =/= FU_LSU &&
    io.fu_in.bits.ops.fu_type =/= FU_MDU)
  val is_lsu_load = io.fu_in.bits.ops.fu_type === FU_LSU &&
    io.fu_in.bits.ops.fu_op.asTypeOf(new LSUOp).func === MX_RD
  psu.io.fu_in.valid := io.fu_in.valid && to_psu
  psu.io.fu_in.bits.wb := io.fu_in.bits.wb
  psu.io.fu_in.bits.wb.v := is_lsu_load || io.fu_in.bits.wb.v
  psu.io.fu_in.bits.fu_type := io.fu_in.bits.ops.fu_type
  psu.io.can_log_now := io.can_log_now

  /* LSMDU IO */
  io.fu_in.ready := (to_lsu && lsu.io.fu_in.ready) ||
    (to_mdu && mdu.io.fu_in.ready) ||
    (to_psu && psu.io.fu_in.ready)
  io.wb.valid := lsu.io.fu_out.valid || mdu.io.fu_out.valid || psu.io.fu_out.valid
  io.wb.bits := Mux1H(Array(
    lsu.io.fu_out.valid -> lsu.io.fu_out.bits,
    mdu.io.fu_out.valid -> mdu.io.fu_out.bits,
    psu.io.fu_out.valid -> psu.io.fu_out.bits))

  if (conf.log_LSMDU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "LSMDU")
    printv(lsu.io.fu_in, "LSMDU.lsu_in")
    printv(lsu.io.fu_out, "LSMDU.lsu_out")
    printv(io.wb, "LSMDU.wb")
    printv(io.fu_in, "LSMDU.fu_in")
  }
  assert (AtMost1H(lsu.io.fu_out.valid, mdu.io.fu_out.valid, psu.io.fu_out.valid))
}
