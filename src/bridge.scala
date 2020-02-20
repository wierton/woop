package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._
import njumips.dumps._
import njumips.utils._

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

  io.in.req.ready := Mux(io.in.req.bits.is_cached, io.cached.req.ready, io.uncached.req.ready)
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

/* assume memory request reach in order */
class CrossbarNx1(m:Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(m, Flipped(new MemIO))
    val out = new MemIO
  })

  val in_valids = Reverse(Cat(for (i <- 0 until m) yield io.in(i).req.valid))
  val in_readys = Reverse(Cat(for (i <- 0 until m) yield io.in(i).req.ready))
  val in_resp_readys = Reverse(Cat(for (i <- 0 until m) yield io.in(i).resp.ready))
  val in_valids_1H = BitsOneWay(in_valids)
  val in_req = Mux1H(for (i <- 0 until m) yield in_valids_1H(i) -> io.in(i).req.bits)

  /* q_datas [0:head, ..., nstages-1:tail] */
  val nstages = 25
  val q_data_tail = RegInit(~(0.U(log2Ceil(m+1).W)))
  val q_datas = Mem(nstages, UInt(m.W))
  val p_data = q_datas(q_data_tail)
  when (io.out.req.fire()) {
    for (i <- 1 until nstages) {
      q_datas(i) := q_datas(i - 1)
    }
    q_datas(0) := in_valids
  }
  q_data_tail := q_data_tail + io.out.req.fire() - io.out.resp.fire()

  for (i <- 0 until m) {
    io.in(i).req.ready := io.out.req.ready && in_valids_1H(i)
    io.in(i).resp.valid := io.out.resp.valid && p_data(i)
    io.in(i).resp.bits := io.out.resp.bits
  }

  io.out.resp.ready := (p_data & in_resp_readys).orR
  io.out.req.valid := in_valids.orR
  io.out.req.bits := in_req

  if (conf.log_CrossbarNx1) {
    dump("crossbar")
  }
  assert ((~q_data_tail).orR =/= 0.U || !io.out.resp.valid)

  def dump(msg:String) = {
    for (i <- 0 until io.in.size) {
      io.in(i).dump(msg+".io.in."+i)
    }
    io.out.dump(msg+".io.out")
    in_req.dump(msg+".in_req")
    printf("%d: "+msg+": in_valids=%b, in_valids_1H=%b, in_readys=%b, in_resp_readys=%b\n", GTimer(), in_valids, in_valids_1H, in_readys, in_resp_readys)
    val p = Seq[Bits](GTimer(), q_data_tail, p_data)
    val q = for (i <- 0 until nstages) yield q_datas(i)
    printf("%d: "+msg+": q_data_tail=%d, p_data=%b, q_datas={"+List.fill(nstages)("%b,").mkString+"}\n", (p++q):_*)
  }
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
  val cached_in_req = RegEnable(next=Mux1H(for (i <- 0 until m) yield in_valids_1H(i) -> io.in(i).req.bits), enable=has_req)
  val cached_out_valids = RegEnable(next=Reverse(Cat(for (i <- 0 until n) yield
      nAddrSpace(i).st <= cached_in_req.addr &&
      cached_in_req.addr < nAddrSpace(i).ed
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
    io.out(i).resp.ready := resping
    io.out(i).req.valid := reqing && cached_out_valids(i)
    io.out(i).req.bits.is_aligned := cached_in_req.is_aligned
    io.out(i).req.bits.is_cached := cached_in_req.is_cached
    io.out(i).req.bits.addr := cached_in_req.addr
    io.out(i).req.bits.data := cached_in_req.data
    io.out(i).req.bits.func := cached_in_req.func
    io.out(i).req.bits.wstrb := cached_in_req.wstrb
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
    cached_in_req.dump(msg+".cached_in_req")
    printf("%d: "+msg+": in_valids=%b, in_valids_1H=%b, cached_in_valids_1H=%b, has_req=%d, in_readys=%b\n", GTimer(), in_valids, in_valids_1H, cached_in_valids_1H, has_req, in_readys)
    printf("%d: "+msg+": reqing=%b, has_resp=%b, resping=%b, working=%b, cached_out_valids=%b\n", GTimer(), reqing, has_resp, resping, working, cached_out_valids)
  }
}
