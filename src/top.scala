package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

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

  crossbar.io.out.req <> dev.io.in.req

  val cistern = Module(new Cistern(new MemResp, 20))
  cistern.io.enq <> dev.io.in.resp
  crossbar.io.out.resp <> cistern.io.deq

  if (conf.log_Cistern) {
    cistern.io.enq.dump("cistern.io.enq")
    cistern.io.deq.dump("cistern.io.deq")
  }

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

import njumipsTest._

object Main extends App {
  chisel3.Driver.execute(args, () => new SOC_EMU_TOP);
  // chisel3.Driver.execute(args, () => new SOC_EMU_TOP);
}
