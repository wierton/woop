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
  })

  io.commit := DontCare

  val rf = Module(new RegFile)
  val ifu = Module(new IFU)
  val bridu = Module(new BRIDU)
  val pralu = Module(new PRALU)
  val lsmdu = Module(new LSMDU)

  ifu.io.fu_out <> bridu.io.fu_in
  bridu.io.fu_out <> pralu.io.fu_in
  pralu.io.fu_out <> lsmdu.io.fu_in
  lsmdu.io.fu_out <> rf.io.wb

  rf.io.rfreq <> bridu.io.rfreq
  ifu.io.iaddr <> pralu.io.iaddr
}
