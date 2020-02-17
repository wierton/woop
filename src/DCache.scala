package MipsNPC

import chisel3._
import chisel3.util._

import Configure._
import ModuleConsts._
import ModuleIO._
import MemConsts._

case class CacheParam(id_width:Int, data_width:Int)

class DCache(param:CacheParam) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new AXI4IO(param.id_width, param.data_width)
  })

  io.out.ar.valid := io.in.req.valid
  io.out.ar.bits.id := 0.U
  io.out.ar.bits.addr := io.in.req.bits.addr
  io.out.ar.bits.len := 0.U
  io.out.ar.bits.size := 0.U
  io.out.ar.bits.burst := 1.U
  io.out.ar.bits.lock := false.B; io.out.ar.bits.cache := 0.U
  io.out.ar.bits.prot := 0.U; io.out.ar.bits.region := 0.U
  io.out.ar.bits.qos := 0.U; io.out.ar.bits.user := 0.U

  io.out.aw.valid := false.B
  io.out.aw.bits.id := 0.U
  io.out.aw.bits.addr := 0.U
  io.out.aw.bits.len := 0.U
  io.out.aw.bits.size := 0.U
  io.out.aw.bits.burst := 1.U
  io.out.aw.bits.lock := false.B; io.out.aw.bits.cache := 0.U
  io.out.aw.bits.prot := 0.U; io.out.aw.bits.region := 0.U
  io.out.aw.bits.qos := 0.U; io.out.aw.bits.user := 0.U

  io.out.w.valid := false.B
  io.out.w.bits.id := 0.U
  io.out.w.bits.data := 0.U
  io.out.w.bits.strb := 0.U
  io.out.w.bits.last := false.B

  io.out.r.ready := true.B
  io.out.b.ready := true.B
}
