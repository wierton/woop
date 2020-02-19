package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._
import DumpUtils._

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
  val imux = Module(new MemMux("imux"))
  val dmux = Module(new MemMux("dmux"))
  val dev = Module(new SimDev)
  val as = Array(new AddrSpace("h00000000".U, "h20000000".U))
  val crossbar = Module(new CrossbarNx1(4))

  dev.io.clock := clock
  dev.io.reset := reset

  imux.io.in <> core.io.imem
  dmux.io.in <> core.io.dmem

  crossbar.io.in(0) <> imux.io.cached
  crossbar.io.in(1) <> imux.io.uncached
  crossbar.io.in(2) <> dmux.io.cached
  crossbar.io.in(3) <> dmux.io.uncached

  crossbar.io.out <> dev.io.in

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
