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
    val in = Flipped(new AXI4IO(4, conf.xprlen))
    val out = new MemIO
  })
}

class MemCrossbar(m:Int, nAddrSpace:Array[AddrSpace]) extends Module {
  val n = nAddrSpace.length
  val io = IO(new Bundle {
    val in = Vec(m, Flipped(new MemIO))
    val out = Vec(n, new MemIO)
  })

  val in_valids = Cat(for (i <- 0 until m) yield io.in(i).req.valid)
  val in_readys = Cat(for (i <- 0 until m) yield io.in(i).req.ready)
  val in_valids_1H = BitsOneWay(in_valids)
  val has_req = (in_valids_1H & in_readys).orR

  val reqing = RegEnable(next=Y, enable=has_req)
  val resping = RegInit(N)
  val working = reqing || resping

  val cached_in_valids_1H = RegEnable(next=in_valids_1H, enable=has_req)
  val cached_req = RegEnable(next=Mux1H(for (i <- 0 until m) yield in_valids_1H(i) -> io.in(i).req.bits), enable=has_req)

  val cached_out_valids = RegEnable(next=Cat(for (i <- 0 until n) yield
      nAddrSpace(i).st <= cached_req.addr &&
      cached_req.addr < nAddrSpace(i).ed
    ), enable=has_req)

  for (i <- 0 until m) {
    io.in(i).req.ready := !working && in_valids_1H(i)
    io.in(i).resp.valid := resping && cached_in_valids_1H(i)
    io.in(i).resp.bits := Mux1H(for (i <- 0 until n) yield
      cached_out_valids(i) -> io.out(i).resp.bits)

    when (io.in(i).resp.fire()) { resping := N }
  }

  val out_req_fire = for (i <- 0 until n) yield io.out(i).req.fire()
  assert (AtMost1H(out_req_fire:_*))
  for (i <- 0 until n) {
    io.out(i).resp.ready := reqing
    io.out(i).req.valid := reqing && cached_out_valids(i)
    io.out(i).req.bits.is_aligned := cached_req.is_aligned
    io.out(i).req.bits.addr := cached_req.addr - nAddrSpace(i).st
    io.out(i).req.bits.data := cached_req.data
    io.out(i).req.bits.func := cached_req.func
    io.out(i).req.bits.wstrb := cached_req.wstrb
    when (io.out(i).req.fire()) {
      reqing := N
      resping := Y
    }
  }

  /* no matched output */
  when (reqing && !cached_out_valids.orR) { reqing := N }
}

