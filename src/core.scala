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

  ifu.fu_out <> bridu.fu_in
  bridu.fu_out <> pralu.fu_in
  pralu.fu_out <> lsmdu.fu_in
  lsmdu.fu_out <> rf.wb

  rf.rfreq <> bridu.rfreq
  ifu.iaddr <> pralu.iaddr
}
