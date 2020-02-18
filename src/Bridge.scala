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

  val valids = Cat(for (i <- 0 until m) yield io.in(i).req.valid)
  val valids_1H = BitsOneWay(valids)
  val has_req = valids_1H.orR

  val reqing = RegEnable(next=Y, enable=has_req)
  val resping = RegInit(N)
  val working = reqing || resping
  val resp_data = WireInit(0.U.asTypeOf(io.in(0).resp.bits))

  val cached_valids_1H = RegEnable(next=valids_1H, enable=has_req)
  val cached_req = RegEnable(next=Mux1H(for (i <- 0 until m) yield valids_1H(i) -> io.in(i).req.bits), enable=has_req)

  for (i <- 0 until m) {
    io.in(i).req.ready := !working && valids_1H(i)
    io.in(i).resp.valid := resping && cached_valids_1H(i)
    io.in(i).resp.bits := resp_data

    when (io.in(i).resp.fire()) { resping := N }
  }

  val out_req_valids = for (i <- 0 until n) yield io.out(i).req.fire()
  assert (AtMost1H(out_req_valids:_*))
  for (i <- 0 until n) {
    io.out(i).resp.ready := reqing
    io.out(i).req.valid := reqing &&
      nAddrSpace(i).st <= cached_req.addr &&
      cached_req.addr < nAddrSpace(i).ed
    io.out(i).req.bits := cached_req
    when (io.out(i).req.fire()) {
      reqing := N
      resping := Y
      resp_data := io.out(i).resp.bits
    }
  }

  /* no matched output */
  when (reqing && !Cat(out_req_valids).orR) {
    reqing := N
  }
}

