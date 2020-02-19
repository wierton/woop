package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._
import DumpUtils._

class SimDDR extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new MemIO)
  })
}

class SimDev extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new MemIO)
  })
}

class SOC_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
  })

  val core = Module(new Core)
  val as = Array(
    new AddrSpace("h00000000".U, "h08000000".U),
    new AddrSpace("h10000000".U, "h20000000".U))
  val ddr = Module(new SimDDR)
  val dev = Module(new SimDev)
  val crossbar = Module(new MemCrossbar(2, as))

  ddr.io.clock := clock
  ddr.io.reset := reset
  dev.io.clock := clock
  dev.io.reset := reset

  crossbar.io.in(0) <> core.io.imem
  crossbar.io.in(1) <> core.io.dmem
  crossbar.io.out(0) <> ddr.io.in
  crossbar.io.out(1) <> dev.io.in

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
