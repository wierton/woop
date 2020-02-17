package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._

class SOC_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val ddr = new MemIO
    val mmio = new MemIO
    val commit = new CommitIO
  })

  val core = Module(new Core)
  val mc = Module(new SRAM_EMU_MC)

  core.io.imem <> mc.io.imem
  core.io.dmem <> mc.io.dmem
  core.io.commit <> io.commit

  mc.io.ddr <> io.ddr
  mc.io.mmio <> io.mmio
}

object Main extends App {
  chisel3.Driver.execute(args, () => new SOC_EMU_TOP);
}
