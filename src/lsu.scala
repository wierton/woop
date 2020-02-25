package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._

class LSUOp extends Bundle {
  val align = UInt(1.W)
  val func  = UInt(1.W)
  val dt    = UInt(2.W)
  val ext   = UInt(1.W)

  def isAligned()  = align =/= 0.U
}

class LSUStage2Data extends Bundle {
  val op = new LSUOp
  val instr = new Instr
  val rd_idx = UInt(REG_SZ.W)
  val pc = UInt(conf.xprlen.W)
  val data = UInt(conf.xprlen.W)
  val addr = UInt(conf.xprlen.W)
  val is_cached = Bool()

  def load(ind:PRALU_LSMDU_IO) = {
    this := Cat(
      ind.ops.fu_op,
      ind.wb.instr.asUInt,
      ind.wb.rd_idx,
      ind.wb.pc,
      ind.ops.op2,
      ind.paddr,
      ind.is_cached
    ).asTypeOf(this)
  }
}

class LSU extends Module with LSUConsts {
  val io = IO(new Bundle {
    val dmem = new MemIO
    val fu_in = Flipped(DecoupledIO(new PRALU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
  })

  io.fu_in.ready := !io.fu_in.valid || io.dmem.req.ready

  /* stage 2: send memory request */
  val s2_in = Wire(new LSUStage2Data)
  s2_in.load(io.fu_in.bits)
  val s2_datas = Module(new Queue(new LSUStage2Data, conf.mio_cycles))
  s2_datas.io.enq.valid := io.dmem.req.fire()
  s2_datas.io.enq.bits := s2_in
  s2_datas.io.deq.ready := io.dmem.resp.fire()
  io.working := s2_datas.io.deq.valid
  io.dmem.req.valid := io.fu_in.valid
  io.dmem.req.bits.is_cached := s2_in.is_cached
  io.dmem.req.bits.is_aligned := s2_in.op.isAligned()
  io.dmem.req.bits.addr  := Mux(s2_in.op.isAligned(), s2_in.addr, s2_in.addr & ~(3.U(32.W)))
  io.dmem.req.bits.func  := s2_in.op.func
  // L: 0 -> b1111, 1 -> b1110, 2 -> b1100, 3 -> b1000
  // R: 0 -> b0001, 1 -> b0011, 2 -> b0111, 3 -> b1111
  io.dmem.req.bits.wstrb := Mux(s2_in.op.isAligned(),
   0.U, Mux(s2_in.op.ext === LSU_L,
     "b1111000".U >> (~s2_in.addr(1, 0)),
     "b0001111".U >> (~s2_in.addr(1, 0))))
  io.dmem.req.bits.data := s2_in.data
  io.dmem.resp.ready := Y

  /* stage 3: recv contents and commit */
  val s3_in = s2_datas.io.deq.bits
  val s3_data = io.dmem.resp.bits.data
  /* io.fu_out.bits.wb.data */
  val lstrb = "b1111000".U >> (~s3_in.addr(1, 0))
  val lmask = Cat(for (i <- 0 until 4) yield lstrb(i).asTypeOf(SInt(8.W)))
  val rstrb = "b0001111".U >> (~s3_in.addr(1, 0))
  val rmask = Cat(for (i <- 0 until 4) yield rstrb(i).asTypeOf(SInt(8.W)))
  io.fu_out.valid := io.dmem.resp.valid
  io.fu_out.bits.pc := s3_in.pc
  io.fu_out.bits.wen := Y
  io.fu_out.bits.rd_idx := s3_in.rd_idx
  io.fu_out.bits.instr := s3_in.instr
  io.fu_out.bits.data := Mux(s3_in.op.isAligned(),
    io.dmem.resp.bits.data, Mux(s3_in.op.ext === LSU_L,
    (lmask & s3_data) | (~lmask & s3_in.data),
    (rmask & s3_data) | (~rmask & s3_in.data)))

  if (conf.log_LSU) {
    printf("%d: LSU: lstrb=%b, lmask=%b, rstrb=%b, rmask=%b, s3_data=%x\n", GTimer(), lstrb, lmask, rstrb, rmask, s3_data)
    s2_in.dump("LSU.s2_in")
    s2_datas.io.enq.dump("LSU.s2_datas.enq")
    s2_datas.io.deq.dump("LSU.s2_datas.deq")
    io.fu_in.dump("LSU.io.in")
    io.fu_out.dump("LSU.io.out")
    io.dmem.dump("LSU.io.dmem")
  }
}

