package woop
package core

import chisel3._
import chisel3.util._
import woop.configs._
import woop.utils._
import woop.consts._

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
    val can_log_now = Input(Bool())
  })

  val core = Module(new Core)
  val dev = Module(new SimDev)
  val crossbar = Module(new CrossbarNx1(2))
  // val icache = Module(new SimICache)
  val icache = Module(new IMemCistern(conf.icache_stages))

  core.io.can_log_now := io.can_log_now
  crossbar.io.can_log_now := io.can_log_now
  icache.io.can_log_now := io.can_log_now

  dev.io.clock := clock
  dev.io.reset := reset

  icache.io.br_flush := core.io.br_flush
  icache.io.ex_flush := core.io.ex_flush

  icache.io.in <> core.io.imem

  icache.io.out <> crossbar.io.in(0)
  core.io.dmem  <> crossbar.io.in(1)

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit

  if (conf.log_Top) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("------------\n")
  }
}

class AXI4_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
  })

  val core = Module(new Core)
  val imux = Module(new MemMux("imux"))
  val dmux = Module(new MemMux("dmux"))
  val dev = Module(new SimDev)
  val crossbar = Module(new CrossbarNx1(4))
  val icache = Module(new ICache)
  val i2sram = Module(new AXI42SRAM)
  val dcache = Module(new DCache)
  val d2sram = Module(new AXI42SRAM)

  dev.io.clock := clock
  dev.io.reset := reset

  imux.io.in <> core.io.imem
  dmux.io.in <> core.io.dmem
  imux.io.cached <> icache.io.in
  icache.io.out <> i2sram.io.in
  dmux.io.cached <> dcache.io.in
  dcache.io.out <> d2sram.io.in

  icache.io.flush := core.io.br_flush
  dcache.io.flush := core.io.br_flush

  i2sram.io.out    <> crossbar.io.in(0)
  imux.io.uncached <> crossbar.io.in(1)
  d2sram.io.out    <> crossbar.io.in(2)
  dmux.io.uncached <> crossbar.io.in(3)

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit
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

object Main {
  def main(args:Array[String]):Unit = {
    val top = args(0)
    val chiselArgs = args.slice(1, args.length)
    chisel3.Driver.execute(chiselArgs, () => {
      val clazz = Class.forName("woop.core."+top)
      val constructor = clazz.getConstructor()
      constructor.newInstance().asInstanceOf[Module]
    })
  }
}
