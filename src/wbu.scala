package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.utils._

class WBU(nexus:Int) extends Module {
  val io = IO(new Bundle {
    val exus = Vec(nexus, Flipped(DecoupledIO(new EXU_WBU_IO)))
    val brinfo = Flipped(ValidIO(new BRINFO_IO))
    val wb = ValidIO(new WriteBackIO)
    val flush = ValidIO(new FlushIO)
  })

  /* register writeback */
  val seq_fu_valids = for (i <- 0 until io.exus.length) yield io.exus(i).valid
  assert(AtMost1H(seq_fu_valids:_*))

  val fu_valids = RegNext(next=Cat(seq_fu_valids))
  val fu_in = VecInit(for (i <- 0 until io.exus.length) yield RegNext(io.exus(i).bits.wb))

  for (i <- 0 until io.exus.length) { io.exus(i).ready := Y }

  io.wb.valid := fu_valids.orR
  io.wb.bits := Mux1H(for (i <- 0 until io.exus.length) yield fu_valids(i) -> fu_in(i))

  /* pc flush */
  io.flush.valid := RegNext(io.brinfo.valid, init=N)
  io.flush.bits.br_target := RegNext(io.brinfo.bits.br_target)
}

