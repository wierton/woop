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

class DeviceAccessorModuleIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new MemIO)
  val enable_bug = Input(Bool())
  val can_log_now = Input(Bool())
}

class DeviceAccessor extends Module {
  val io = IO(new DeviceAccessorModuleIO)

  val dev = Module(new SimDev)
  dev.io.clock := io.clock
  dev.io.reset := io.reset

  val lfsr = LFSR16(io.in.asUInt.xorR)
  val delay = RegInit(0.U(4.W))
  if (conf.random_delay) {
    io.in.req <> dev.io.in.req
    when (io.in.req.fire()) { delay := lfsr }
    .otherwise {
      delay := Mux(delay === 0.U, 0.U, delay - 1.U)
    }

    val timeout = delay === 0.U
    dev.io.in.resp.ready := io.in.resp.ready && timeout
    io.in.resp.valid := dev.io.in.resp.valid && timeout
    io.in.resp.bits := dev.io.in.resp.bits
  } else {
    io.in <> dev.io.in
  }

  if (conf.log_DeviceAccessor) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "DeviceAccessor")
  }
}

class Divider extends Module {
  val io = IO(Flipped(new DividerIO))
  val dividend = io.data_dividend_tdata.asSInt
  val divisor = io.data_divisor_tdata.asSInt
  val quotient = (dividend / divisor).asUInt
  val remainder = (dividend % divisor).asUInt
  require(quotient.getWidth == 40)
  require(remainder.getWidth == 40)
  val pipe = Pipe(io.data_dividend_tvalid && io.data_divisor_tvalid,
    Cat(quotient, remainder), conf.div_stages)

  io.data_dividend_tready := Y
  io.data_divisor_tready := Y
  io.data_dout_tvalid := pipe.valid
  io.data_dout_tdata := pipe.bits
}

class Multiplier extends Module {
  val io = IO(Flipped(new MultiplierIO))
  val a = io.data_a.asSInt
  val b = io.data_b.asSInt
  val pipe = Pipe(Y, (a * b).asUInt, conf.mul_stages)

  io.data_dout := pipe.bits
}

class verilator_top_io extends Bundle {
  val commit = new CommitIO
  val can_log_now = Input(Bool())
  val enable_bug = Input(Bool())
}

class verilator_top extends Module {
  val io = IO(new verilator_top_io)

  val core = Module(new Core)
  val dev = Module(new SimDev)
  val crossbar = Module(new CrossbarNx1(2))
  val icache = Module(new SimICache)
  val divider = Module(new Divider)
  val multiplier = Module(new Multiplier)
  // val icache = Module(new IMemCistern(conf.icache_stages))

  // dev.io.can_log_now := io.can_log_now
  core.io.can_log_now := io.can_log_now
  core.io.enable_bug := io.enable_bug
  crossbar.io.can_log_now := io.can_log_now
  icache.io.can_log_now := io.can_log_now

  dev.io.clock := clock
  dev.io.reset := reset

  icache.io.br_flush := core.io.br_flush
  icache.io.ex_flush := core.io.ex_flush

  divider.io <> core.io.divider
  multiplier.io <> core.io.multiplier

  icache.io.in <> core.io.imem

  icache.io.out <> crossbar.io.in(0)
  core.io.dmem  <> crossbar.io.in(1)

  icache.io.control <> core.io.icache_control

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit

  if (conf.log_Top) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "verilator_top")
  }
}

class AXI4_EMU_TOP_ModuleIO extends Bundle {
  val commit = new CommitIO
}

class AXI4_EMU_TOP extends Module {
  val io = IO(new AXI4_EMU_TOP_ModuleIO)

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

  icache.io.br_flush := core.io.br_flush
  icache.io.ex_flush := core.io.ex_flush
  dcache.io.flush := core.io.br_flush

  i2sram.io.out    <> crossbar.io.in(0)
  imux.io.uncached <> crossbar.io.in(1)
  d2sram.io.out    <> crossbar.io.in(2)
  dmux.io.uncached <> crossbar.io.in(3)

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit
}

class loongson_top_top extends Bundle {
  val imem = new AXI4IO(conf.xprlen)
  val dmem = new AXI4IO(conf.xprlen)
  val divider = new DividerIO
  val multiplier = new MultiplierIO
  val commit = new CommitIO
}

class loongson_top extends Module {
  val io = IO(new loongson_top_top)

  val core = Module(new Core)
  val imem2axi = Module(new MemIO2AXI(conf.xprlen))
  val dmem2axi = Module(new MemIO2AXI(conf.xprlen))
  val icache = Module(new SimICache)

  core.io.can_log_now := N
  icache.io.can_log_now := N

  core.io.imem <> icache.io.in
  icache.io.out <> imem2axi.io.in
  core.io.dmem <> dmem2axi.io.in
  core.io.divider <> io.divider
  core.io.multiplier <> io.multiplier
  imem2axi.io.out <> io.imem
  dmem2axi.io.out <> io.dmem

  icache.io.control <> core.io.icache_control
  icache.io.ex_flush := core.io.ex_flush
  icache.io.br_flush := core.io.br_flush

  io.commit <> core.io.commit
}

class zedboard_top extends loongson_top {
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
