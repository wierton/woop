package MipsNPC
package IO

import chisel3._
import chisel3.util._

import Configure._
import Consts._

class Instr {
  val op     = Wire(UInt(OP_SZ.W));
  val rs_idx = Wire(UInt(REG_SZ.W));
  val rt_idx = Wire(UInt(REG_SZ.W));
  val rd_idx = Wire(UInt(REG_SZ.W));
  val shamt  = Wire(UInt(SHAMT_SZ.W));
  val func   = Wire(UInt(FUNC_SZ.W));

  def imm    = Cat(rd_idx, shamt, func);
  def addr   = Cat(rs_idx, rt_idx, imm);

  private val SeqAll = Seq(op, rs_idx, rt_idx, rd_idx, shamt, func);

  def this(in:UInt) {
    this();
    Tie(SeqAll:_*) := in;
  }

  def := (in:UInt):Unit = {
    Tie(SeqAll:_*) := in;
  }

  def asUInt() = Cat(SeqAll);
}

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


class CommitIO extends Bundle {
  val valid = Output(Bool())
  val pc = Output(UInt(conf.xprlen.W))
  val instr = Output(UInt(conf.xprlen.W))
  val gpr = Output(Vec(32, UInt(conf.xprlen.W)))
}

class CoreIO extends Bundle {
  val imem = new MemIO
  val dmem = new MemIO
  val commit = new CommitIO
}

class FlushIO extends Bundle {
  val br_target = Output(UInt(conf.xprlen.W));
}

class BypassIO extends Bundle {
  val wen = Output(Bool()); // need bypass
  val reg_dest_idx = Output(UInt(REG_SZ.W));
  val data = Output(UInt(conf.xprlen.W));
}

class WriteBackIO extends BypassIO {
  val npc = Output(UInt(conf.xprlen.W));
}

class IFU_IDU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W));
  val instr = Output(UInt(conf.xprlen.W));
}

class IDU_ISU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W));
  val instr = Output(UInt(conf.xprlen.W));
  val fu_type = Output(UInt(FU_TYPE_SZ.W))
  val fu_op = Output(UInt(FU_OP_SZ.W));
  val op1_sel = Output(UInt(OP1_SEL_SZ.W));
  val op2_sel = Output(UInt(OP2_SEL_SZ.W));
  val dest_sel = Output(UInt(DEST_SEL_SZ.W));
}

//========================================================
//         ISU --> {ALU, LSU, MDU, BRU}
//========================================================
class ISU_ALU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W));
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val op1 = Output(UInt(conf.xprlen.W));
  val op2 = Output(UInt(conf.xprlen.W));
  val reg_dest_idx = Output(UInt(REG_SZ.W));
}

class ISU_MDU_IO extends ISU_ALU_IO { }

class ISU_BRU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W));
  val npc = Output(UInt(conf.xprlen.W)); // PC next
  val rs_data = Output(UInt(conf.xprlen.W));
  val rt_data = Output(UInt(conf.xprlen.W));
  val addr = Output(UInt(ADDR_SZ.W));
  val se_off = Output(UInt(conf.xprlen.W));
  val reg_dest_idx = Output(UInt(REG_SZ.W));
}

class ISU_LSU_IO extends Bundle {
  val fu_op = Output(UInt(FU_OP_SZ.W));
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val base = Output(UInt(conf.xprlen.W));
  val offset = Output(UInt(conf.xprlen.W));
  val data = Output(UInt(conf.xprlen.W));
  val reg_dest_idx = Output(UInt(REG_SZ.W));
}


//========================================================//
//        {ALU, LSU, MDU, BRU}  -->  WBU           //
//========================================================//
class LSU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_wb = Output(Bool());
  val reg_dest_idx = Output(UInt(REG_SZ.W));
  val data = Output(UInt(conf.xprlen.W));
}

class ALU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val reg_dest_idx = Output(UInt(REG_SZ.W));
  val data = Output(UInt(conf.xprlen.W));
}

class MDU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_wb = Output(Bool());
  val reg_dest_idx = Output(UInt(REG_SZ.W));
  val data = Output(UInt(conf.xprlen.W));
}

class BRU_WBU_IO extends Bundle {
  val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
  val need_br = Output(Bool());
  val br_target = Output(UInt(conf.xprlen.W));
  val need_wb = Output(Bool());
  val reg_dest_idx = Output(UInt(REG_SZ.W));
  val data = Output(UInt(conf.xprlen.W));
}

//========================================================//
//               xxx --> GPIO --> xxx                     //
//========================================================//
class GPIO_OUT extends Bundle {
  val halted = Output(Bool());
  val code = Output(UInt(conf.xprlen.W));
  val reason = Output(UInt(conf.xprlen.W));
}

class IDU_GPIO_IO extends Bundle {
  val halted = Input(Bool());
  val invalid_instr = Output(Bool());
  val npc = Output(UInt(conf.xprlen.W));
  val instr = Output(UInt(conf.xprlen.W));
}

class CROSSBAR_GPIO_IO extends Bundle {
  val invalid_address = Output(Bool());
  val addr = Output(UInt(conf.xprlen.W));
}
