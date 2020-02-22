package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

class RegFile extends Module {
  val io = IO(new Bundle {
    val bp = Flipped(ValidIO(new BypassIO))
    val wb = Flipped(ValidIO(new WriteBackIO))
    val rfreq = Flipped(new RegFileReq)
  })

  val wb_rf = Mem(32, UInt(conf.xprlen.W))
  val bp_rf = Mem(32, UInt(conf.xprlen.W))
  val rf_dirtys = Mem(32, Bool())
  val bp_readys = Mem(32, Bool())

  def bypass_match(idx:UInt) = io.bp.valid && io.bp.bits.rd_idx === idx
  def rf_data_ready(idx:UInt) = !rf_dirtys(idx) || bp_readys(idx) || bypass_match(idx)
  def rf_data_bits(idx:UInt) = MuxCase(0.U, Array(
    !rf_dirtys(idx) -> wb_rf(idx),
    bypass_match(idx) -> io.bp.bits.data,
    bp_readys(idx) -> bp_rf(idx)))

  io.rfreq.rs_data.valid := rf_data_ready(io.rfreq.rs_idx)
  io.rfreq.rs_data.bits := rf_data_bits(io.rfreq.rs_idx)

  io.rfreq.rt_data.valid := rf_data_ready(io.rfreq.rt_idx)
  io.rfreq.rt_data.bits := rf_data_bits(io.rfreq.rt_idx)

  when (io.rfreq.dest_ridx.valid) {
    rf_dirtys(io.rfreq.dest_ridx.bits) := Y
    bp_readys(io.bp.bits.rd_idx) := N
  }

  when (io.wb.valid) {
    when (io.wb.bits.wen) {
      wb_rf(io.wb.bits.rd_idx) := io.wb.bits.data
    }
    rf_dirtys(io.wb.bits.rd_idx) := N
  }

  when (io.bp.valid) {
    when (io.bp.bits.wen) {
      bp_rf(io.bp.bits.rd_idx) := io.bp.bits.data
    }
    bp_readys(io.bp.bits.rd_idx) := Y
  }
}
