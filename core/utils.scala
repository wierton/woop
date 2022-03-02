package woop
package utils

import chisel3._
import chisel3.util._

import woop.configs._
import java.lang.reflect._

object GTimer {
  def apply(): UInt = {
    val (t, c) = Counter(true.B, 0x7fffffff)
    t
  }
}

class ROB[T<:Data](gen:T, idw:Int, nEnq:Int=1) extends Module {
  class ROBEnq extends Bundle {
    val id = Output(UInt(idw.W))
    val data = Output(gen)
  }

  val io = IO(new Bundle {
    val enq = Vec(nEnq, Flipped(ValidIO(new ROBEnq)))
    val deq = DecoupledIO(gen)
    val flush = Input(Bool())
  })

  val entries = 1 << idw
  val queue = Mem(entries, ValidIO(gen))
  val head = RegInit(0.U(log2Ceil(entries).W))
  val q_head = queue(head)

  io.deq.valid := q_head.valid
  io.deq.bits := q_head.bits
  when (io.deq.fire()) {
    q_head.valid := false.B
    head := head + 1.U
  }

  for (i <- 0 until nEnq) {
    when (io.enq(i).valid) {
      val q_in = queue(io.enq(i).bits.id)
      q_in.valid := true.B
      q_in.bits := io.enq(i).bits.data
    }
  }

  when (io.flush) {
    head := 0.U
    for (i <- 0 until entries) { queue(i).valid := false.B }
  }

  when (reset.toBool) {
    for (i <- 0 until entries) { queue(i).valid := false.B }
  }
}

class SeqMemAccessor[T<:Data](n:Int, gen:T) extends Module {
  val io = IO(new Bundle {
    val wen = Input(Bool())
    val raddr = Input(UInt(log2Ceil(n).W))
    val rdata = Output(gen)
    val waddr = Input(UInt(log2Ceil(n).W))
    val wdata = Input(gen)
  })

  val mem = SyncReadMem(n, gen)
  when (io.wen) { mem.write(io.waddr, io.wdata) }
  io.rdata := mem.read(io.raddr, !io.wen)
}

class Cistern[T<:Data](gen:T, entries:Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val can_log_now = Input(Bool())
  })

  val onoff = RegInit(true.B)
  val size = RegInit(0.U(log2Ceil(entries + 1).W))
  val queue = Module(new Queue(gen, entries))
  size := size + queue.io.enq.fire() - queue.io.deq.fire()
  queue.io.enq <> io.enq

  io.deq.bits := queue.io.deq.bits
  io.deq.valid := queue.io.deq.valid && onoff
  queue.io.deq.ready := io.deq.ready && onoff
  when (size === entries.U) {
    onoff := true.B
  } .elsewhen(size === 0.U) {
    onoff := false.B
  }

  if (conf.log_Cistern) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
  }
}

// filter bits, left 1 way
object BitsOneWay {
  def apply(data:UInt):UInt = {
    if (data.getWidth <= 1) {
      data
    } else {
      val OneToN = Cat(for (i <- 1 until data.getWidth) yield
        (data(i) && !Cat(for (i <- 0 until i) yield
          data(i)).orR))
      Cat(Reverse(OneToN), data(0))
    }
  }
}

object Only1H {
  // Only1H(a_valid, b_valid) -> a & b' | a' & b
  def apply(data:Bool*) = {
    Cat(for(i <- 0 until data.length) yield
      Cat(for(j <- 0 until data.length) yield
        if(i == j) data(j).asUInt.orR else !(data(j).asUInt)).andR
      ).orR
  }
}

object AtMost1H {
  def apply(data:Bool*) = !Cat(data).orR || Only1H(data:_*)
}

object CLZ_32 {
  def apply(in: UInt):UInt = {
    val out = Wire(Vec(5, Bool()))

    out(4) := in(31, 16) === 0.U(16.W)

    val val16 = Mux(out(4), in(15, 0), in(31, 16))
    out(3) := val16(15, 8) === 0.U(8.W)

    val val8  = Mux(out(3), val16(7, 0), val16(15, 8))
    out(2) := val8(7, 4) === 0.U(4.W)

    val val4  = Mux(out(2), val8(3, 0), val8(7, 4))
    out(1) := val4(3, 2) === 0.U(2.W)

    out(0) := Mux(out(1), ~val4(1), ~val4(3))

    Mux(in === 0.U, 32.U, out.asUInt)
  }
}

object CLO_32 {
  def apply(in: UInt):UInt = {
    val out = Wire(Vec(5, Bool()))

    out(4) := in(31, 16) === ~0.U(16.W)

    val val16 = Mux(out(4), in(15, 0), in(31, 16))
    out(3) := val16(15, 8) === ~0.U(8.W)

    val val8  = Mux(out(3), val16(7, 0), val16(15, 8))
    out(2) := val8(7, 4) === ~0.U(4.W)

    val val4  = Mux(out(2), val8(3, 0), val8(7, 4))
    out(1) := val4(3, 2) === ~0.U(2.W)

    out(0) := Mux(out(1), val4(1), val4(3))

    Mux(in === ~0.U(32.W), 32.U, out.asUInt)
  }
}

/*
object BundleDump {
  import scala.reflect.runtime.{universe => ru}

  def dump[T<:Data](obj:T) = {
    val decls = ru.typeOf[T].decls()
    val fmts:String = ""
    val sigs:Seq[Bits] = Seq[Bits]()
    for (decl in fields) {
      String name = field.getName()
      Object value = field.get(obj)

      if (value.getType <:< ru.typeOf[Data]) {
        if (value.getType <:< ru.typeOf[UInt]) {
        }
      }
    }
    (fmts, sigs)
  }

  def apply[T<:Data](sig:T, msg:String):Unit = {
    val fmt = "%d: " + msg + ": "
    val sig = Seq[Bits](GTimer())
    val _1, _2 = dump(sig)
    fmt = fmt +  _1
    sig = sig ++ _2
    printf(fmt + "\n", sig:_*)
  }
}
*/
