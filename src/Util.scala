package Configure

import chisel3._
import chisel3.util._

object conf {
  val xprlen = 32;
  val addr_width = 32;
  val data_width = 32;
  val xprbyte = xprlen / 8;
  val start_addr = "h10000000".U;
  val axi_data_width = 32;
  val axi_id_width = 4;
  val log = true;
  val diff = true;
}

object log {
  def apply(fmt:String, data:Bits*) = {
    if(conf.log) {
      printf(fmt, data:_*);
    }
  }
}

object IFill {
  def apply(data:UInt, n:Int) = {
    Cat(for(i <- 0 until n) yield Fill(n, data(i)));
  }
}

/*
object MuxN {
  def apply(n:Int, data:UInt, sel:UInt*):UInt = {
    assert(data.getWidth == n);
    if(n > 1)
      return Mux(data(n - 1),
        MuxN(n - 1, data(n - 2, 0), sel.slice(n - 1, 0)),
        MuxN(n - 1, data(n - 2, 0), sel.slice(2 * n - 1, n))
        );
      return Mux(data(0), sel(0), sel(1));
  }
}

object Mux2 {
  def apply(data:Data, sel:Data*) = MuxN(2, data, sel:_*);
}
*/

object CyclicShift {
  implicit class RShift(lhs:UInt) {
    def %>>(rhs:UInt) = {
      ((lhs >> rhs) | ((lhs << 1) << ~rhs))(lhs.getWidth - 1, 0);
    }
  }

  implicit class LShift(lhs:UInt) {
    def %<<(rhs:UInt) = ((lhs << rhs) | ((lhs >> 1) >> ~rhs))(lhs.getWidth - 1, 0);
  }
}


object ExtOperation {
  implicit class BitsExtTo[T<:Bits](in:T) {
    def ZExt(n:Int):UInt = {
      assert(n >= in.getWidth);
      Cat(Fill(n - in.getWidth, 0.U(1.W)), in);
    }

    def SExt(n:Int):UInt = {
      assert(n >= in.getWidth);
      Cat(Fill(n - in.getWidth, in.head(1)), in);
    }
  }
}

import ExtOperation._
import CyclicShift._

class Tie(data:Bits*) {
  def :=(in:Bits) {
    var total:Int = (for(d <- data) yield d.getWidth).reduce(_ + _);
    assert(total == in.getWidth);
    for(d <- data) {
      d := in(total - 1, total - d.getWidth);
      total = total - d.getWidth;
    }
  }
}

object Tie {
  def apply(data:Bits*) = new Tie(data:_*);
}

object Only1H {
  // Only1H(a_valid, b_valid) -> a & b' | a' & b
  def apply(data:Bits*) = {
    Cat(for(i <- 0 until data.length) yield
      Cat(for(j <- 0 until data.length) yield
        if(i == j) data(j).asUInt.orR else !(data(j).asUInt)).andR
      ).orR;
  }
}

object AtMost1H {
  def apply(data:Bits*) = !Cat(data).orR || Only1H(data:_*);
}
