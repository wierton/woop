package MipsNPC

import chisel3._
import chisel3.util._

import ModuleConsts._
import MemConsts._
import Configure._
import ModuleIO._

class AddrSpace(start:UInt, end:UInt) {
  val st = start
  val ed = end
}

class AXI42SRAM extends Module {
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

  val req_is_valid = RegEnable(next=Y, enable=io.imem.req.fire() || io.dmem.req.fire(), init=N)
  val imem = RegEnable(next=io.imem, enable=io.imem.req.fire(), init=0.U.asTypeOf(io.imem))
  val dmem = RegEnable(next=io.dmem, enable=io.dmem.req.fire(), init=0.U.asTypeOf(io.dmem))

  val req = Mux(imem.req.valid, imem, dmem)
  val resp_is_imem = RegEnable(next=Y, enable=io.bram.req.valid || io.ddr.req.valid || io.mmio.req.valid, init=N)

  val data_valid = io.ddr.resp.fire() || io.mmio.resp.fire()
  val data = Mux(io.ddr.resp.fire(), io.ddr.resp.bits.data,
    Mux(io.mmio.resp.fire(), io.mmio.resp.bits.data, 0.U))

  io.imem.req.ready := !req_is_valid
  io.dmem.req.ready := !req_is_valid && !io.imem.req.valid

  assert(!(io.ddr.resp.fire() && io.mmio.resp.fire()))

  io.ddr.req.valid := 0x80000000.U <= req.addr && req.addr < 0x90000000.U
  io.ddr.req.bits := req.bits
  io.ddr.resp.ready := Y

  io.mmio.req.valid := 0XA0000000.U <= req.addr && req.addr < 0XC0000000.U && !io.bram.req.valid
  io.mmio.req.bits := req.bits
  io.mmio.resp.ready := Y
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
