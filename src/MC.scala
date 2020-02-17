package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class AddrSpace(start:UInt, end:UInt) {
  val st = start
  val ed = end
}

class AXI42SRAM extends Module {
  val io = IO(new Bundle {
  })
}

class MemCrossbar(m:Int, nAddrSpace:Array[AddrSpace]) extends Module {
  val io = IO(new Bundle {
    val in = Vec(m, Flipped(new MemIO))
    val out = Vec(nAddrSpace.length, new MemIO)
  })
}

class SRAM_EMU_MC extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new MemIO)
    val dmem = Flipped(new MemIO)
    val ddr  = new MemIO
    val mmio = new MemIO
  })
}

class AXI4_EMU_MC extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

class ZEDBOARD_MC extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

class LOONGSON_MC extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}
