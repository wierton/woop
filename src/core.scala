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

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU(4)) // rf
  val lsu = Module(new LSU)
  val alu = Module(new ALU)
  val mdu = Module(new MDU)
  val bru = Module(new BRU)
  val pru = Module(new PRU)
  val wbu = Module(new WBU(4)) // rf

  ifu.io.iaddr <> pru.io.iaddr
  lsu.io.daddr <> pru.io.daddr

  // bypass signals
  isu.io.bypasses(0) <> alu.io.bypass
  isu.io.bypasses(1) <> mdu.io.bypass
  isu.io.bypasses(2) <> lsu.io.bypass
  isu.io.bypasses(3) <> bru.io.bypass

  lsu.io.bp_failed := bru.io.bp_failed

  /* branch info */
  wbu.io.brinfo <> bru.io.brinfo

  // Flush signals
  ifu.io.flush <> wbu.io.flush
  idu.io.flush <> wbu.io.flush
  isu.io.flush <> wbu.io.flush
  lsu.io.flush <> wbu.io.flush
  alu.io.flush <> wbu.io.flush
  mdu.io.flush <> wbu.io.flush
  bru.io.flush <> wbu.io.flush

  ifu.io.imem <> io.imem
  lsu.io.dmem <> io.dmem

  ifu.io.idu <> idu.io.ifu
  idu.io.isu <> isu.io.idu

  // ISU -> XXX
  isu.io.alu <> alu.io.isu
  isu.io.mdu <> mdu.io.isu
  isu.io.lsu <> lsu.io.isu
  isu.io.bru <> bru.io.isu

  // XXX -> WBU
  lsu.io.wbu <> wbu.io.exus(0)
  alu.io.wbu <> wbu.io.exus(1)
  mdu.io.wbu <> wbu.io.exus(2)
  bru.io.wbu <> wbu.io.exus(3)

  isu.io.wb <> wbu.io.wb
}
