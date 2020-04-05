package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

import woop.utils._

class INTRU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new EXU_LSMDU_IO))
    val fu_out = DecoupledIO(new EXU_LSMDU_IO)
    val cp0 = Flipped(new CP0_INTRU_IO)
    val can_log_now = Input(Bool())
  })

  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire())
  io.fu_in.ready := !fu_valid || (io.fu_out.ready && !io.cp0.intr)

  io.fu_out.valid := fu_valid
  io.fu_out.bits := fu_in
  io.fu_out.bits.wb.ip7 := io.cp0.ip7
  io.cp0.valid := fu_valid

  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen (io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_INTRU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "INTRU")
  }
}
