package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._

import woop.configs._
import woop.utils._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val icache_control = ValidIO(new CacheControl)
    val commit = new CommitIO
    val br_flush = Output(Bool())
    val ex_flush = Output(Bool())
    val multiplier = new MultiplierIO
    val divider = new DividerIO
    val can_log_now = Input(Bool())
    val enable_bug = Input(Bool())
  })

  val rf  = Module(new RegFile)
  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val exu = Module(new EXU)
  val ehu = Module(new EHU)
  val cp0 = Module(new CP0)
  val tlb = Module(new TLB)
  val msu = Module(new MSU)

  /* log */
  rf.io.can_log_now := io.can_log_now
  ifu.io.can_log_now := io.can_log_now
  idu.io.can_log_now := io.can_log_now
  exu.io.can_log_now := io.can_log_now
  exu.io.enable_bug := io.enable_bug
  cp0.io.can_log_now := io.can_log_now
  tlb.io.can_log_now := io.can_log_now
  ehu.io.can_log_now := io.can_log_now
  msu.io.can_log_now := io.can_log_now

  /* pipeline */
  ifu.io.fu_out <> idu.io.fu_in
  idu.io.fu_out <> exu.io.fu_in
  exu.io.fu_out <> ehu.io.fu_in
  ehu.io.fu_out <> msu.io.fu_in

  /* time intr */
  ehu.io.cp0 <> cp0.io.ehu

  /* writeback and bypass */
  msu.io.wb <> rf.io.wb
  msu.io.wb <> exu.io.wb
  exu.io.bp <> rf.io.bp
  idu.io.rfio <> rf.io.rfio

  /* cp0 */
  exu.io.cp0_rport <> cp0.io.rport
  exu.io.cp0_wport <> cp0.io.wport

  /* TLB */
  ifu.io.iaddr <> tlb.io.iaddr
  exu.io.daddr <> tlb.io.daddr
  exu.io.cp0_tlbr_port <> cp0.io.tlbr_port
  exu.io.cp0_tlbw_port <> cp0.io.tlbw_port
  exu.io.cp0_tlbp_port <> cp0.io.tlbp_port
  exu.io.tlb_rport <> tlb.io.rport
  exu.io.tlb_wport <> tlb.io.wport
  exu.io.tlb_pport <> tlb.io.pport

  /* mem */
  ifu.io.imem <> io.imem
  msu.io.dmem <> io.dmem

  /* commit */
  io.commit <> rf.io.commit

  /* flush */
  io.br_flush := idu.io.br_flush.valid
  io.ex_flush := cp0.io.ex_flush.valid
  ifu.io.br_flush <> idu.io.br_flush
  ifu.io.ex_flush <> cp0.io.ex_flush
  idu.io.ex_flush <> cp0.io.ex_flush
  exu.io.ex_flush <> cp0.io.ex_flush
  ehu.io.ex_flush <> cp0.io.ex_flush
  rf.io.ex_flush <> cp0.io.ex_flush
  tlb.io.br_flush <> idu.io.br_flush
  tlb.io.ex_flush <> cp0.io.ex_flush

  tlb.io.status := cp0.io.status

  msu.io.multiplier <> io.multiplier
  msu.io.divider <> io.divider

  io.icache_control <> exu.io.icache_control
}
