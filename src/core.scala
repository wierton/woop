package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.dumps._
import woop.configs._
import woop.utils._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val commit = new CommitIO
    val br_flush = Output(Bool())
    val ex_flush = Output(Bool())
    val can_log_now = Input(Bool())
  })

  val rf = Module(new RegFile)
  val ifu = Module(new IFU)
  val bridu = Module(new BRIDU)
  val pralu = Module(new PRALU)
  val lsmdu = Module(new LSMDU)

  rf.io.can_log_now := io.can_log_now
  ifu.io.can_log_now := io.can_log_now
  bridu.io.can_log_now := io.can_log_now
  pralu.io.can_log_now := io.can_log_now
  lsmdu.io.can_log_now := io.can_log_now

  ifu.io.fu_out <> bridu.io.fu_in
  bridu.io.fu_out <> pralu.io.fu_in
  pralu.io.fu_out <> lsmdu.io.fu_in
  lsmdu.io.fu_out <> rf.io.wb
  lsmdu.io.fu_out <> pralu.io.wb

  rf.io.bp <> pralu.io.bp
  rf.io.rfio <> bridu.io.rfio
  ifu.io.iaddr <> pralu.io.iaddr

  ifu.io.imem <> io.imem
  lsmdu.io.dmem <> io.dmem

  ifu.io.br_flush <> bridu.io.br_flush
  pralu.io.br_flush <> bridu.io.br_flush
  ifu.io.ex_flush <> pralu.io.ex_flush
  bridu.io.ex_flush <> pralu.io.ex_flush
  rf.io.ex_flush <> pralu.io.ex_flush

  pralu.io.rs_data <> rf.io.rfio.rs_data
  pralu.io.rt_data <> rf.io.rfio.rt_data

  io.br_flush := bridu.io.br_flush.valid
  io.ex_flush := pralu.io.ex_flush.valid
  io.commit <> rf.io.commit
}
