package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class LSMDU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRIDU_IN))
    val fu_out = ValidIO(new WriteBackIO)
  })

  /* LSU IO */
  val LSU = Module(new LSU)
  lsu.io.fu_in.valid := io.fu_in.valid && io.fu_in.fu_type === FU_LSU
  lsu.io.fu_in.bits.wb <> io.fu_out.wb
  lsu.io.fu_in.bits.ops := io.fu_in.ops
  lsu.io.fu_out.ready := io.fu_out.ready

  /* MDU IO */
  val mdu = Module(new MDU)
  mdu.io.fu_in.valid := io.fu_in.valid && io.fu_in.fu_type === FU_MDU
  mdu.io.fu_in.bits.wb <> io.fu_out.wb
  mdu.io.fu_in.bits.ops := io.fu_in.ops
  mdu.io.fu_out.ready := io.fu_out.ready

  /* pipeline stage for ALU,BRU,PRU */
  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen(io.fu_in.fire() && io.fu_in.ops.fu_type =/= FU_LSU || io.fu_in.ops.fu_type =/= FU_MDU) {
    fu_valid := Y
  }

  /* LSMDU IO */
  io.fu_in.ready := lsu.io.fu_in.ready && mdu.io.fu_in.ready
  io.fu_out.valid := lsu.io.fu_out.valid || mdu.io.fu_out.valid || fu_valid
  io.fu_out.wb := Mux1H(Array(
    lsu.io.fu_out.valid -> lsu.io.fu_out.wb,
    mdu.io.fu_out.valid -> mdu.io.fu_out.wb,
    fu_valid -> fu_in.wb))
  assert (AtMost1H(alu.io.fu_out.valid, pru.io.fu_out.valid, fu_valid))
}
