package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._

class Core extends Module {
  val io = new CoreIO

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU) // rf
  val lsu = Module(new LSU)
  val alu = Module(new ALU)
  val mdu = Module(new MDU)
  val bru = Module(new BRU)
  val wbu = Module(new WBU) // rf

  // bypass signals
  isu.io.alu_bypass <> alu.io.bypass
  isu.io.mdu_bypass <> mdu.io.bypass
  isu.io.lsu_bypass <> lsu.io.bypass
  isu.io.bru_bypass <> bru.io.bypass

  // Flush signals
  ifu.io.flush <> wbu.io.flush
  idu.io.flush <> wbu.io.flush
  isu.io.flush <> wbu.io.flush
  lsu.io.flush <> wbu.io.flush
  alu.io.flush <> wbu.io.flush
  mdu.io.flush <> wbu.io.flush
  bru.io.flush <> wbu.io.flush

  ifu.io.mem <> io.imem
  lsu.io.mem <> io.dmem

  ifu.io.idu <> idu.io.ifu
  idu.io.isu <> isu.io.idu

  // ISU -> XXX
  isu.io.alu <> alu.io.isu
  isu.io.mdu <> mdu.io.isu
  isu.io.lsu <> lsu.io.isu
  isu.io.bru <> bru.io.isu

  // XXX -> WBU
  lsu.io.wbu <> wbu.io.lsu
  alu.io.wbu <> wbu.io.alu
  mdu.io.wbu <> wbu.io.mdu
  bru.io.wbu <> wbu.io.bru

  isu.io.wbu <> wbu.io.wb
}
