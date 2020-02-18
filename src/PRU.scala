package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

class TLBRequest extends Bundle {
  val fcn = Output(Bool()) // 1: load, 0:store
  val vaddr = Output(UInt(conf.xprlen.W))
}

class TLBResponse extends Bundle {
  val paddr = Output(UInt(conf.xprlen.W))
  val off = Output(UInt(16.W))
  val etw = Output(UInt(ETW_WIDTH.W))
}

class TLBTransaction extends Bundle {
  // when tlbx is executing, the req should be hanged
  val req = DecoupledIO(new TLBRequest)
  val resp = Flipped(ValidIO(new TLBResponse))
}

class PRU extends Module {
  val io = IO(new Bundle {
    val iaddr = new TLBTransaction
    val daddr = new TLBTransaction
  })

}

