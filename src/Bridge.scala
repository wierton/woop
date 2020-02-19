package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._
import DumpUtils._

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

class MemMux(name:String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val cached = new MemIO
    val uncached = new MemIO
  })

  io.in.req.ready := io.cached.req.ready && io.uncached.req.ready
  io.in.resp.valid := io.cached.resp.valid || io.uncached.resp.valid
  io.in.resp.bits := Mux1H(Array(
    io.cached.resp.valid -> io.cached.resp.bits,
    io.uncached.resp.valid -> io.uncached.resp.bits))
  assert (!io.cached.resp.valid || !io.uncached.resp.valid)

  io.cached.req.valid := io.in.req.valid && io.in.req.bits.is_cached
  io.cached.req.bits := io.in.req.bits
  io.cached.resp.ready := io.in.resp.ready

  io.uncached.req.valid := io.in.req.valid && !io.in.req.bits.is_cached
  io.uncached.req.bits := io.in.req.bits
  io.uncached.resp.ready := io.in.resp.ready
}

class MemCrossbar(m:Int, nAddrSpace:Array[AddrSpace]) extends Module {
  val n = nAddrSpace.length
  val io = IO(new Bundle {
    val in = Vec(m, Flipped(new MemIO))
    val out = Vec(n, new MemIO)
  })

  val in_valids = Reverse(Cat(for (i <- 0 until m) yield io.in(i).req.valid))
  val in_readys = Reverse(Cat(for (i <- 0 until m) yield io.in(i).req.ready))
  val in_valids_1H = BitsOneWay(in_valids)
  val has_req = (in_valids_1H & in_readys).orR

  val reqing = RegEnable(next=Y, enable=has_req, init=N)
  val resping = RegInit(N)
  val working = reqing || resping

  val cached_in_valids_1H = RegEnable(next=in_valids_1H, enable=has_req)
  val cached_req = RegEnable(next=Mux1H(for (i <- 0 until m) yield in_valids_1H(i) -> io.in(i).req.bits), enable=has_req)

  val cached_out_valids = RegEnable(next=Reverse(Cat(for (i <- 0 until n) yield
      nAddrSpace(i).st <= cached_req.addr &&
      cached_req.addr < nAddrSpace(i).ed
    )), enable=has_req)
  val has_resp = Cat(for (i <- 0 until n) yield io.out(i).resp.valid).orR

  for (i <- 0 until m) {
    io.in(i).req.ready := !working && in_valids_1H(i)
    io.in(i).resp.valid := has_resp && resping && cached_in_valids_1H(i)
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
    io.out(i).req.bits.is_cached := cached_req.is_cached
    io.out(i).req.bits.addr := cached_req.addr
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

  dump("crossbar")

  def dump(msg:String) = {
    for (i <- 0 until io.in.size) {
      io.in(i).dump(msg+".io.in."+i)
    }
    for (i <- 0 until io.out.size) {
      io.out(i).dump(msg+".io.out."+i)
    }
    cached_req.dump(msg+".cached_req")
    printf("%d: "+msg+": in_valids=%b, in_valids_1H=%b, cached_in_valids_1H=%b, has_req=%d, in_readys=%b\n", GTimer(), in_valids, in_valids_1H, cached_in_valids_1H, has_req, in_readys)
    printf("%d: "+msg+": reqing=%b, has_resp=%b, resping=%b, working=%b, cached_out_valids=%b\n", GTimer(), reqing, has_resp, resping, working, cached_out_valids)
  }
}

