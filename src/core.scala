package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val commit = new CommitIO
    val flush = Output(Bool())
  })

  val rf = Module(new RegFile)
  val ifu = Module(new IFU)
  val bridu = Module(new BRIDU)
  val pralu = Module(new PRALU)
  val lsmdu = Module(new LSMDU)

  ifu.io.fu_out <> bridu.io.fu_in
  bridu.io.fu_out <> pralu.io.fu_in
  pralu.io.fu_out <> lsmdu.io.fu_in
  lsmdu.io.fu_out <> rf.io.wb

  rf.io.bp <> pralu.io.bp
  rf.io.rfreq <> bridu.io.rfreq
  ifu.io.iaddr <> pralu.io.iaddr

  ifu.io.imem <> io.imem
  lsmdu.io.dmem <> io.dmem

  ifu.io.br_flush <> bridu.io.br_flush
  ifu.io.ex_flush <> pralu.io.ex_flush
  bridu.io.ex_flush <> pralu.io.ex_flush
  io.flush := bridu.io.br_flush.valid || pralu.io.ex_flush.valid

  pralu.io.rs_data := rf.io.rfreq.rs_data
  pralu.io.rt_data := rf.io.rfreq.rt_data

  io.commit <> rf.io.commit
}
