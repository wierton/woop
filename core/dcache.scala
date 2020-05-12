package woop
package core

import chisel3._
import chisel3.util._
import woop.configs._
import woop.consts._

class DCache extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new AXI4IO(conf.xprlen)
    val flush = Input(Bool())
  })

  io.in <> DontCare
  io.out <> DontCare
}
