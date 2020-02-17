package MipsNPC

import chisel3._
import chisel3.util._

import Configure._
import ModuleConsts._
import ModuleIO._

object MemConsts {
  // MX
  val MX_SZ = 1
  val MX_X  = 0.U(MX_SZ.W)
  val MX_RD = 0.U(MX_SZ.W)
  val MX_WR = 1.U(MX_SZ.W)

  val MT_SZ = 2
  val MT_X  = 0.U(MT_SZ.W)
  val MT_B  = 1.U(MT_SZ.W)
  val MT_H  = 2.U(MT_SZ.W)
  val MT_W  = 3.U(MT_SZ.W)
}

import MemConsts._

class MemReq extends Bundle {
  val addr = Output(UInt(conf.xprlen.W)); // enable s
  val data  = Output(UInt(conf.xprlen.W))
  val func  = Output(UInt(MX_SZ.W))
  val wstrb = Output(UInt((conf.xprlen / 8).W))
}

class MemResp extends Bundle {
  val data = Output(UInt(conf.xprlen.W))
}

class MemIO extends Bundle {
  val req = DecoupledIO(new MemReq)
  val resp = Flipped(DecoupledIO(new MemResp))
}

trait MemoryMapped {
  def io_in = Flipped(new MemIO)
  val start_addr:Int = 0
  val end_addr:Int = 0
}


class SimAXI4Memory(id_width:Int, data_width:Int) extends Module {
  val io = IO(Flipped(new AXI4IO(id_width, data_width)))
  val mem_size = 128 * 1024 * 1024
  val mem = Mem(mem_size, UInt(8.W))

  /////////////////////////////////////////////////////////////
  //////////////////          Read        /////////////////////
  /////////////////////////////////////////////////////////////
  val ar = RegInit(io.ar.bits)
  val ar_valid = RegEnable(next=true.B, enable=io.ar.fire(), init=false.B)
  val ar_data = RegInit(0.U(data_width.W))
  val ar_last = RegInit(true.B)

  io.ar.ready := ar_last

  io.r.valid := ar_valid
  io.r.bits.id := ar.id
  io.r.bits.data := ar_data
  io.r.bits.resp := 0.U
  io.r.bits.last := ar_last
  io.r.bits.user := 0.U

  assert(io.ar.bits.burst === 1.U)

  private def read(in:AXI4_AR) {
    val burst_bytes = 1.U(1.W) << in.size
    assert((burst_bytes << 3) <= data_width.U)
    printf("READ: r_burst_bytes:%x\n", burst_bytes)

    // bytes
    val r_data = Cat(for(i <- (data_width / 8 - 1) to 0 by -1) yield Mux(i.U < burst_bytes, mem(in.addr + i.U), 0.U(8.W))).asUInt

    printf("READ: addr:%x, size:%x, len:%x, data:%x\n", in.addr, in.size, in.len, r_data)

    ar_data := r_data
    ar.addr := in.addr + burst_bytes
    ar.size := in.size
    ar.len := in.len - 1.U
    ar.id := in.id
    ar_last := in.len === 0.U
  }

  // cycle when fire, prepare data
  when(io.ar.fire()) { read(io.ar.bits); }

  // cycle after fire, prepare burst data
  //   if next_cycle_burst_read is true, the ar can't fire
  when(!ar_last && io.r.fire()) { read(ar); }

  // last burst cycle
  when(ar_last && !io.ar.fire()) {
    ar_valid := false.B
  }



  /////////////////////////////////////////////////////////////
  //////////////////         Write        /////////////////////
  /////////////////////////////////////////////////////////////
  val aw = RegInit(io.aw.bits)
  val b_valid = RegInit(false.B)
  val aw_burst = RegInit(false.B)
  val aw_data = RegInit(0.U(data_width.W))
  val w_last = io.w.fire() && io.w.bits.last

  io.aw.ready := !aw_burst
  io.w.ready := aw_burst || io.aw.fire()

  io.b.valid := b_valid
  io.b.bits.id := aw.id
  io.b.bits.resp := 0.U
  io.b.bits.user := 0.U

  when(w_last) { b_valid := true.B; }
  when(io.b.fire() && !w_last) { b_valid := false.B; }

  printf("aw_burst:%x, w_last:%x\n", aw_burst, w_last)

  private def write(in:AXI4_AW, d:AXI4_W) {
    val burst_bytes = (1.U(1.W) << in.size)
    assert((burst_bytes << 3) < data_width.U)
    assert(in.burst === 1.U)
    // update registers
    aw.id := in.id
    aw.addr := in.addr + burst_bytes
    // actual write operation
    for(i <- 0 until data_width / 8) {
      when(i.U < burst_bytes && d.strb(i) === 1.U) {
        printf("write(%x, %x)\n", in.addr + i.U, d.data((i+1)*8-1, i*8))
        mem(in.addr + i.U) := d.data((i+1)*8-1, i*8)
      }
    }
    aw_burst := !d.last
  }

  // the first only-aw cycle
  when(io.aw.fire() && !io.w.fire()) { aw := io.aw.bits; }
  // first with-aw write
  when(io.aw.fire() && io.w.fire()) { write(io.aw.bits, io.w.bits); }
  // remained write
  when(aw_burst && io.w.fire()) { write(aw, io.w.bits); }
}

