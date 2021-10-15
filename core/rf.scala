package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

import woop.utils._

class RegFile extends Module {
  val io = IO(new Bundle {
    val bp = Flipped(ValidIO(new BypassIO))
    val wb = Flipped(ValidIO(new WriteBackIO))
    val rfio = Flipped(new RegFileIO)
    val commit = Output(new CommitIO)
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
  })

  val wbids = Mem(32, UInt(conf.INSTR_ID_SZ.W))
  val wb_rf = Mem(32, UInt(conf.xprlen.W))
  val bp_rf = Mem(32, UInt(conf.xprlen.W))
  val rf_dirtys = Mem(32, Bool())
  val bp_readys = Mem(32, Bool())

  when (reset.toBool) {
    for (i <- 0 until 32) {
      wbids(i) := 0.U
      rf_dirtys(i) := 0.U
      bp_readys(i) := 0.U
      wb_rf(i) := 0.U
    }
  }

  def bypass_match(idx:UInt) = io.bp.valid && io.bp.bits.v && io.bp.bits.wen && io.bp.bits.rd_idx === idx
  def rf_data_ready(idx:UInt) = !rf_dirtys(idx) || bp_readys(idx) || bypass_match(idx) || idx === 0.U
  def rf_data_bits(idx:UInt) = MuxCase(0.U, Array(
    (idx === 0.U) -> 0.U,
    !rf_dirtys(idx) -> wb_rf(idx),
    bypass_match(idx) -> io.bp.bits.data,
    bp_readys(idx) -> bp_rf(idx)))

  io.rfio.rs_data.valid := rf_data_ready(io.rfio.rs_idx)
  io.rfio.rs_data.bits := rf_data_bits(io.rfio.rs_idx)

  io.rfio.rt_data.valid := rf_data_ready(io.rfio.rt_idx)
  io.rfio.rt_data.bits := rf_data_bits(io.rfio.rt_idx)

  when (io.wb.valid && io.wb.bits.v) {
    when (io.wb.bits.wen && io.wb.bits.rd_idx =/= 0.U) {
      wb_rf(io.wb.bits.rd_idx) := io.wb.bits.data
    }
    when (wbids(io.wb.bits.rd_idx) === io.wb.bits.id) {
      rf_dirtys(io.wb.bits.rd_idx) := N
    }
  }

  when (io.bp.valid && io.bp.bits.v && io.bp.bits.wen &&
    io.bp.bits.rd_idx =/= 0.U) {
    bp_rf(io.bp.bits.rd_idx) := io.bp.bits.data
    bp_readys(io.bp.bits.rd_idx) := Y
  }

  /* sequence matter */
  when (io.rfio.wen) {
    rf_dirtys(io.rfio.rd_idx) := Y
    bp_readys(io.rfio.rd_idx) := N
    wbids(io.rfio.rd_idx) := io.rfio.wid
  }

  when (io.ex_flush.valid) {
    for (i <- 0 until 32) {
      rf_dirtys(i) := N
      bp_readys(i) := Y
    }
  }

  io.commit.valid := io.wb.valid
  io.commit.pc := io.wb.bits.pc
  io.commit.instr := io.wb.bits.instr.asUInt
  io.commit.ip7 := io.wb.bits.ip7
  io.commit.wen := io.wb.bits.v && io.wb.bits.wen
  io.commit.wdata := io.wb.bits.data
  io.commit.rd_idx := io.wb.bits.rd_idx
  for (i <- 0 until 32) {
    io.commit.gpr(i) := Mux(io.wb.valid && io.wb.bits.wen && io.wb.bits.rd_idx === i.U, io.wb.bits.data, wb_rf(i))
  }

  if (conf.log_rf) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(io.commit, "RF.io.commit")
    printv(io.bp, "RF.io.bp")
    printv(io.wb, "RF.io.wb")
    printv(io.rfio, "RF.io.rfio")
    printv(io.commit, "RF.io.commit")
    printv(io.ex_flush, "RF.io.ex_flush")
    printv.memdump(wbids, "RF.io.wbids")
  }
}
