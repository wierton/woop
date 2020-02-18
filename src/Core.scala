package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

/* pipeline:
 * 
 * fire  WORKING fire   -> fire   WORKING  fire
 * empty WORKING fire   -> ready  WORKING  !valid
 * fire  WORKING !ready -> !ready BLOCKING valid
 *
 * */

class NCyclesModule extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Output(UInt(32.W))))
    val out = DecoupledIO(Output(UInt(32.W)))
  })
  val fu_in = RegEnable(next=io.in.bits, enable=io.in.fire())
  // io.in.ready := random
  io.out.valid := fu_valid
  io.out.bits := fu_in.bits
}

/* ? cycle */
class PiplineUnitAtLeastNCycles extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Output(UInt(32.W))))
    val out = DecoupledIO(Output(UInt(32.W)))
    val flush = Input(Bool())
  })

  val m = Module(new NCyclesModule)
  val fu_valid = RegInit(N)

  io.in.ready := io.out.ready || !fu_valid

  io.out.valid := m.io.valid
  io.out.bits := m.io.bits

  when (io.flush || (!io.in.fire() && io.out.fire())) {
    fu_valid := N
  } .elsewhen (!io.flush && io.in.fire()) {
    fu_valid := Y
  }
}

/* 1 cycle */
class PiplineUnit1Cycle extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Output(UInt(32.W))))
    val out = DecoupledIO(Output(UInt(32.W)))
    val flush = Input(Bool())
  })

  val fu_in = RegEnable(next=io.in.bits, enable=io.in.fire())
  val fu_valid = RegInit(N)

  io.in.ready := io.out.ready || !fu_valid

  io.out.valid := fu_valid
  io.out.bits := fu_in.bits

  when (io.flush || (!io.in.fire() && io.out.fire())) {
    fu_valid := N
  } .elsewhen (!io.flush && io.in.fire()) {
    fu_valid := Y
  }
}

/* n cycle */
class PiplineUnitNCycles extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Output(UInt(32.W))))
    val out = DecoupledIO(Output(UInt(32.W)))
    val flush = Input(Bool())
  })

  val ncycles = 12
  val fu_in = RegEnable(next=io.in.bits, enable=io.in.fire())
  val fu_valids = RegInit(0.U(ncycles.W))

  io.in.ready := io.out.ready || !fu_valids(0)

  io.out.valid := fu_valids(0)
  io.out.bits := Pipe(fu_in.bits, ncycles)

  when (io.flush) {
    fu_valids := 0.U
  } .elsewhen (!io.in.fire() && io.out.fire()) {
    fu_valids := Cat(N, fu_valids >> 1)
  } .elsewhen (io.in.fire()) {
    fu_valids := Cat(Y, fu_valids >> 1)
  }
}

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val commit = new CommitIO
  })

  io.commit := DontCare

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

  ifu.io.imem <> io.imem
  lsu.io.dmem <> io.dmem

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
