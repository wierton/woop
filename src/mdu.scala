package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

class MDUOp extends Bundle
{
  val mf_reg = UInt(1.W)
  val func   = UInt(2.W)
  val sign   = UInt(1.W)
  val wb_reg = UInt(1.W)
}

class MDU extends Module with MDUConsts {
  val io = IO(new Bundle {
    val fu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
  })

  io.fu_in.ready := Y

  io.fu_out.valid := N
  io.fu_out.bits := 0.U.asTypeOf(io.fu_out.bits)
  io.working := N
}
