package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._

abstract class AXI4ChannelA
{
  val id = Output(UInt(conf.AXI4_ID_WIDTH.W))
  val addr = Output(UInt(32.W))
  val len = Output(UInt(8.W))     // burst length
  val size = Output(UInt(3.W))    // size of each transfer: 2^size bytes
  val burst = Output(UInt(2.W))
  val lock = Output(Bool())
  val cache = Output(UInt(4.W))
  val prot = Output(UInt(3.W))
  val region = Output(UInt(4.W))
  val qos = Output(UInt(4.W))
  val user = Output(UInt(5.W))
}

class AXI4ChannelAW extends AXI4ChannelA { }

class AXI4ChannelAR extends AXI4ChannelA { }

class AXI4ChannelW(data_width: Int) extends Bundle
{
  val id = Output(UInt(conf.AXI4_ID_WIDTH.W))
  val data = Output(UInt(data_width.W))
  val strb = Output(UInt((data_width / 8).W))
  val last = Output(Bool())
  val user = Output(UInt(5.W))

  override def cloneType = { new AXI4ChannelW(data_width).asInstanceOf[this.type] }
}

class AXI4ChannelB extends Bundle
{
  val id = Output(UInt(conf.AXI4_ID_WIDTH.W))
  val resp = Output(UInt(2.W))
  val user = Output(UInt(5.W))
}

// read data channel signals
class AXI4ChannelR(data_width: Int) extends Bundle
{
  val id = Output(UInt(conf.AXI4_ID_WIDTH.W))
  val data = Output(UInt(data_width.W))
  val resp = Output(UInt(2.W))
  val last = Output(Bool())
  val user = Output(UInt(5.W))

  override def cloneType = { new AXI4ChannelR(data_width).asInstanceOf[this.type] }
}

class AXI4IO(data_width: Int) extends Bundle 
{
  val aw = DecoupledIO(new AXI4ChannelAW)
  val w = DecoupledIO(new AXI4ChannelW(data_width))
  val b = Flipped(DecoupledIO(new AXI4ChannelB))
  val ar = DecoupledIO(new AXI4ChannelAR)
  val r = Flipped(DecoupledIO(new AXI4ChannelR(data_width)))

  override def cloneType = { new AXI4IO(data_width).asInstanceOf[this.type] }
}
