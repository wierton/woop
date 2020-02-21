package njumips
package core

import chisel3._
import chisel3.util._
import njumips.consts._
import njumips.configs._

class LSUOp extends Bundle
{
  val align = UInt(1.W)
  val func  = UInt(1.W)
  val dt    = UInt(2.W)
  val ext   = UInt(1.W)

  def isAligned()  = align =/= 0.U
  def isRead()   = func === MX_RD
  def isWrite()  = func === MX_WR
  def isSExt()   = ext === LSU_XE
  def isZExt()   = ext === LSU_ZE
  def isLeft()   = ext(0) === LSU_L
  def isRight()  = ext(0) === LSU_R
  def getDtExt() = Cat(dt, ext)
}

class LSUStage2Data extends Bundle {
  val op = new LSUOp
  val rd_idx = UInt(REG_SZ.W)
  val pc = UInt(conf.xprlen.W)
  val data = UInt(conf.xprlen.W)
  val addr = UInt(conf.xprlen.W)
  val is_cached = Bool()

  def load(lsu:ISU_LSU_IO, tlb:TLBResp) = {
    this := Cat(tlb.is_cached, tlb.paddr,lsu.data,
      lsu.pc, lsu.rd_idx, lsu.fu_op).asTypeOf(this)
  }
}

class LSU extends Module with UnitOpConstants {
  val io = IO(new Bundle {
    val dmem = new MemIO
    val daddr = new TLBTransaction
    val isu = Flipped(DecoupledIO(new ISU_LSU_IO))
    val wbu = DecoupledIO(new EXU_WBU_IO)
    val bypass = ValidIO(new BypassIO)
    val bp_failed = Input(Bool())
    val flush = Flipped(ValidIO(new FlushIO))
  })

  /* branch prediction */
 val bp_failed = RegInit(N)
 when (io.flush.valid) { bp_failed := N }
   .elsewhen(io.bp_failed) { bp_failed := Y }

   /* stage 1: synchronize */
  val s1_valid = RegInit(N)
  io.isu.ready := io.dmem.req.ready || !s1_valid
  io.daddr.req.valid := io.isu.valid
  io.daddr.req.bits.func := io.isu.bits.fu_op.asTypeOf(new LSUOp).func
  io.daddr.req.bits.vaddr := io.isu.bits.base + io.isu.bits.offset
  assert (!(s1_valid && io.daddr.req.ready && !io.daddr.resp.fire()))
  when (io.flush.valid || (!io.daddr.req.fire() && io.dmem.req.fire())) {
    s1_valid := N
    } .elsewhen(!io.flush.valid && io.daddr.req.fire()) {
      s1_valid := Y
    }

    /* stage 2: send memory request */
   val mio_cycles = 2
   val s2_in = Wire(new LSUStage2Data)
   s2_in.load(io.isu.bits, io.daddr.resp.bits)
   val s2_datas = Module(new Queue(new LSUStage2Data, mio_cycles))
   val s2_valids = RegInit(0.U(mio_cycles.W))
   s2_datas.reset := io.flush.valid
   s2_datas.io.enq.valid := io.dmem.req.fire()
   s2_datas.io.enq.bits := s2_in
   s2_datas.io.deq.ready := io.dmem.resp.fire()
   io.dmem.req.valid := io.daddr.resp.valid
   io.dmem.req.bits.is_cached := s2_in.is_cached
   io.dmem.req.bits.is_aligned := s2_in.op.isAligned()
   io.dmem.req.bits.addr  := Mux(s2_in.op.isAligned(),
     s2_in.addr, s2_in.addr & ~(3.U(32.W)))
   io.dmem.req.bits.func  := s2_in.op.func
   // L: 0 -> b1111, 1 -> b1110, 2 -> b1100, 3 -> b1000
   // R: 0 -> b0001, 1 -> b0011, 2 -> b0111, 3 -> b1111
   io.dmem.req.bits.wstrb := Mux(s2_in.op.isAligned(),
     0.U, Mux(s2_in.op.ext === LSU_L,
       "b1111000".U >> (~s2_in.addr(1, 0)),
       "b0001111".U >> (~s2_in.addr(1, 0))))
   io.dmem.req.bits.data := s2_in.data
   io.dmem.resp.ready := io.wbu.ready

   /* stage 3: recv contents and commit */
  val s3_in = s2_datas.io.deq.bits
  val s3_data = io.dmem.resp.bits.data
  io.wbu.valid := io.dmem.resp.valid
  io.wbu.bits.wb.pc := s3_in.pc
  io.wbu.bits.wb.wen := Y
  io.wbu.bits.wb.rd_idx := s3_in.rd_idx

  /* io.wbu.bits.wb.data */
  val lstrb = "b1111000".U >> (~s3_in.addr(1, 0))
  val lmask = Cat(for (i <- 0 until 4) yield lstrb(i).asTypeOf(SInt(8.W)))
  val rstrb = "b0001111".U >> (~s3_in.addr(1, 0))
  val rmask = Cat(for (i <- 0 until 4) yield rstrb(i).asTypeOf(SInt(8.W)))
  io.wbu.bits.wb.data := Mux(s3_in.op.isAligned(),
    io.dmem.resp.bits.data, Mux(s3_in.op.ext === LSU_L,
      (lmask & s3_data) | (~lmask & s3_in.data),
      (rmask & s3_data) | (~rmask & s3_in.data)))
  io.wbu.bits.ex := 0.U.asTypeOf(io.wbu.bits.ex)

  /* bypass */
  io.bypass.valid := io.dmem.resp.valid
  io.bypass.bits.wen := Y
  io.bypass.bits.rd_idx := s3_in.rd_idx
  io.bypass.bits.data := io.wbu.bits.wb.data
}

