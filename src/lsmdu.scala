package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

class LSMDU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val dmem = new MemIO
  })

  /* LSU IO */
  val lsu = Module(new LSU)
  lsu.io.fu_in.valid := io.fu_in.valid && io.fu_in.bits.ops.fu_type === FU_LSU
  lsu.io.fu_in.bits := io.fu_in.bits
  lsu.io.fu_out.ready := Y
  lsu.io.dmem <> io.dmem

  /* MDU IO */
  val mdu = Module(new MDU)
  mdu.io.fu_in.valid := io.fu_in.valid && io.fu_in.bits.ops.fu_type === FU_MDU
  mdu.io.fu_in.bits := io.fu_in.bits
  mdu.io.fu_out.ready := Y

  /* pipeline stage for ALU,BRU,PRU */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire() && (io.fu_in.bits.ops.fu_type =/= FU_LSU || io.fu_in.bits.ops.fu_type =/= FU_MDU)) {
    fu_valid := Y
  }

  /* LSMDU IO */
  io.fu_in.ready := lsu.io.fu_in.ready && mdu.io.fu_in.ready
  io.fu_out.valid := lsu.io.fu_out.valid || mdu.io.fu_out.valid || fu_valid
  io.fu_out.bits := Mux1H(Array(
    lsu.io.fu_out.valid -> lsu.io.fu_out.bits.wb,
    mdu.io.fu_out.valid -> mdu.io.fu_out.bits.wb,
    fu_valid -> fu_in.wb))
  assert (AtMost1H(lsu.io.fu_out.valid, mdu.io.fu_out.valid, fu_valid))

  if (conf.log_LSMDU) {
    printf("%d: LSMDU: fu_valid=%b\n", GTimer(), fu_valid)
    io.fu_out.dump("LSMDU.io.fu_out")
    io.fu_in.dump("LSMDU.io.fu_in")
  }
}
