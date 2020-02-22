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
    val bp = Flipped(new BypassIO)
    val wb = Flipped(new WriteBackIO)
    val rs_idx = Input(UInt(REG_SZ.W))
    val rs_data = ValidIO(Output(UInt(conf.xprlen.W)))
    val rt_idx = Input(UInt(REG_SZ.W))
    val rt_data = ValidIO(Output(UInt(conf.xprlen.W)))
    val dest_ridx = ValidIO(Input(UInt(REG_SZ.W)))
  })

  val wb_rf = Mem(32, 0.U(conf.xprlen.W))
  val bp_rf = Mem(32, 0.U(conf.xprlen.W))
  val rf_dirtys = Mem(32, N)
  val bp_readys = Mem(32, N)

  val bypass_match(idx:UInt) = io.bp.valid && io.bp.rd_idx === idx
  val rf_data_ready(idx:UInt) = !rf_dirtys(idx) || bp_readys(idx) || bypass_match(idx)
  val rf_data_bits(idx:UInt) = MuxCase(Array(
    !rf_dirtys(idx) -> wb_rf(idx),
    bypass_match(idx) -> io.bp.bits.data,
    bp_readys(idx) -> bp_rf(idx)))

  io.rs_data.valid := rf_data_ready(io.rs_idx)
  io.rs_data.bits := rf_data_bits(io.rs_idx)

  io.rt_data.valid := rf_data_ready(io.rt_idx)
  io.rt_data.bits := rf_data_bits(io.rt_idx)

  when (io.dest_ridx.valid) {
    rf_dirtys(io.dest_ridx.bits).write(Y)
    bp_readys(io.bp.bits.rd_idx).write(N)
  }

  when (io.wb.valid) {
    when (io.wb.bits.wen) {
      wb_rf(io.wb.bits.rd_idx).write(io.wb.bits.data)
    }
    rf_dirtys(io.wb.bits.rd_idx).write(N)
  }

  when (io.bp.valid) {
    when (io.bp.bits.wen) {
      bp_rf(io.bp.bits.rd_idx).write(io.bp.bits.data)
    }
    bp_readys(io.bp.bits.rd_idx).write(Y)
  }
}
