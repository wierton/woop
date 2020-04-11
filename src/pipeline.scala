package woop
package examples

import chisel3._
import chisel3.util._

import woop.consts._

/* pipeline:
 * 
 * fire  WORKING fire   -> fire   WORKING  fire
 * empty WORKING fire   -> ready  WORKING  !valid
 * fire  WORKING !ready -> !ready BLOCKING valid
 *
 * */

class FU_IN extends Bundle {
  val din = Output(UInt(32.W))
}

class FU_OUT extends Bundle {
  val dout = Output(UInt(32.W))
}

object Compute {
  def apply(din:FU_IN) = din.asTypeOf(new FU_OUT)
  def apply(d1:FU_IN, d2:FU_OUT) = {
    (d1.asUInt ^ d2.asUInt).asTypeOf(new FU_OUT)
  }
  def apply(d1:FU_OUT, d2:FU_IN) = {
    (d1.asUInt ^ d2.asUInt).asTypeOf(new FU_OUT)
  }
}

object Merge {
}

class FixedNCyclesModule(n:Int=1) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
    val flush = Input(Bool())
  })
  io.in.ready := DontCare
  io.out.valid := DontCare
  io.out.bits := DontCare
}

class AtLeastNCyclesModule(n:Int=1) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
  })
  io.in.ready := DontCare
  io.out.valid := DontCare
  io.out.bits := DontCare
}

/* 1 cycle: ALU, IDU, BRU */
abstract class PiplineUnit1Cycle extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
    val flush = Input(Bool())
  })

  val fu_in = RegEnable(next=io.in.bits, enable=io.in.fire(), init=0.U.asTypeOf(io.in.bits))
  val fu_valid = RegInit(N)

  io.in.ready := io.out.ready || !fu_valid

  io.out.valid := fu_valid
  io.out.bits := Compute(fu_in)

  when (io.flush || (!io.in.fire() && io.out.fire())) {
    fu_valid := N
  } .elsewhen (!io.flush && io.in.fire()) {
    fu_valid := Y
  }
}

/* n cycle: MDU */
abstract class PiplineUnitNCycles extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
    val flush = Input(Bool())
  })

  val ncycles = 12
  val fu_in = RegEnable(next=io.in.bits, enable=io.in.fire(), init=0.U.asTypeOf(io.in.bits))
  val fu_valids = RegInit(0.U(ncycles.W))

  io.in.ready := io.out.ready || !fu_valids(0)

  io.out.valid := fu_valids(0)
  io.out.bits := Compute(Pipe(Y, fu_in, ncycles).bits)

  when (io.flush) {
    fu_valids := 0.U
  } .elsewhen (!io.in.fire() && io.out.fire()) {
    fu_valids := Cat(N, fu_valids >> 1)
  } .elsewhen (io.in.fire()) {
    fu_valids := Cat(Y, fu_valids >> 1)
  }
}

/* n cycle with data: IFU, LSU, MDU with IP core */
abstract class PiplineUnitNCyclesWithData extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
    val flush = Input(Bool())
  })

  val ncycles = 12
  val m = Module(new FixedNCyclesModule(ncycles))
  // val m = Module(new AtLeastNCyclesModule(ncycles))
  m.io.in <> io.in
  m.io.flush := io.flush

  val fu_datas = Module(new Queue(new FU_IN, ncycles))
  fu_datas.io.enq.valid := m.io.in.fire()
  fu_datas.io.enq.bits := io.in.bits
  fu_datas.io.deq.ready := m.io.out.fire()

  io.in.ready := m.io.in.ready
  io.out.valid := m.io.out.valid
  io.out.bits := Compute(m.io.out.bits, fu_datas.io.deq.bits)
}

/* at least 1 cycle */
abstract class PiplineUnit1CycleWithData extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FU_IN))
    val out = DecoupledIO(new FU_OUT)
    val flush = Input(Bool())
  })

  val m = Module(new AtLeastNCyclesModule(1))
  val fu_data = RegEnable(next=0.U.asTypeOf(new FU_IN), enable=m.io.in.fire(), init=0.U.asTypeOf(new FU_IN))

  io.in.ready := m.io.in.ready

  m.io.in.valid := io.in.valid
  m.io.in.bits := io.in.bits
  io.in.ready := m.io.in.ready

  m.io.out.ready := io.out.ready
  io.out.valid := m.io.out.valid
  io.out.bits := m.io.out.bits
}
