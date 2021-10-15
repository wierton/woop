package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

import woop.utils._

class EHU extends Module {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new EXU_EHU_IO))
    val fu_out = DecoupledIO(new EHU_MSU_IO)
    val cp0 = new EHU_CP0_IO
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
  })

  val fu_valid = RegInit(N)
  val fu_in = RegEnable(next=io.fu_in.bits, enable=io.fu_in.fire(), init=0.U.asTypeOf(io.fu_in.bits))
  io.fu_in.ready := (!fu_valid || io.fu_out.ready) && !io.ex_flush.valid

  io.fu_out.valid := fu_valid
  io.fu_out.bits.wb := fu_in.wb
  io.fu_out.bits.ops := fu_in.ops
  io.fu_out.bits.is_cached := fu_in.is_cached
  io.fu_out.bits.wb.ip7 := io.cp0.ip7

  /* cp0 */
  io.cp0.valid := io.fu_out.fire()
  io.cp0.wb := io.fu_out.bits.wb
  io.cp0.ex := fu_in.ex

  when (!io.fu_in.fire() && io.fu_out.fire()) {
    fu_valid := N
  } .elsewhen (io.fu_in.fire()) {
    fu_valid := Y
  }

  if (conf.log_EHU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "EHU")
    printv(io.fu_in, "EHU.io.fu_in")
    printv(io.fu_out, "EHU.io.fu_out")
    printv(io.cp0, "EHU.io.cp0")
    printv(io.ex_flush, "EHU.io.ex_flush")
  }
}
