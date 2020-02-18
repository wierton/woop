package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class SOC_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val ddr = new MemIO
    val mmio = new MemIO
    val commit = new CommitIO
  })

  val core = Module(new Core)
  val as = Array(
    new AddrSpace("h00000000".U, "h10000000".U),
    new AddrSpace("h10000000".U, "h20000000".U))
  val crossbar = Module(new MemCrossbar(2, as))

  crossbar.io.in(0) <> core.io.imem
  crossbar.io.in(1) <> core.io.dmem
  crossbar.io.out(0) <> io.ddr
  crossbar.io.out(1) <> io.mmio

  core.io.commit <> io.commit
}

class AXI4_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

class ZEDBOARD_TOP extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

class LOONGSON_TOP extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

import MipsNPCTest._

object Main extends App {
  chisel3.Driver.execute(args, () => new SOC_EMU_TOP);
  // chisel3.Driver.execute(args, () => new SOC_EMU_TOP);
}
