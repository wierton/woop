package Configure

import chisel3._
import chisel3.util._


object GTimer {
  def apply(): UInt = {
    val (t, c) = Counter(true.B, 0x7fffffff)
    t
  }
}

object conf {
  val xprlen = 32;
  val addr_width = 32;
  val data_width = 32;
  val xprbyte = xprlen / 8;
  val start_addr = "hbfc00000".U;
  val axi_data_width = 32;
  val axi_id_width = 4;
  val log = true;
  val diff = true;
}

class Cistern[T<:Data](gen: T, entries: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
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

  printf("%d: Cistern: onoff=%b, size=%d\n", GTimer(), onoff, size)
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
      ).orR;
  }
}

object AtMost1H {
  def apply(data:Bool*) = !Cat(data).orR || Only1H(data:_*);
}
