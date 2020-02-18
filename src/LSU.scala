package MipsNPC

import chisel3._
import chisel3.util._

import Consts._
import Configure._
import IO._

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
  val data = UInt(conf.xprlen.W)
  val addr = UInt(conf.xprlen.W)

  def this(lsu:ISU_LSU_IO, _addr:UInt):Unit {
    this()
    this := Cat(_addr, lsu.data, lsu.rd_idx, lsu.fu_op).asTypeOf(this)
  }
}

class LSU extends Module with UnitOpConsts {
  val io = IO(new Bundle {
    val dmem = new MemIO
    val daddr = new TLBTransaction
    val isu = Flipped(DecoupledIO(new ISU_LSU_IO))
    val wbu = DecoupledIO(new LSU_WBU_IO)
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
  io.daddr.req.bits.vaddr := io.isu.bits.addr + io.isu.bits.offset
  assert (!(s1_valid && io.daddr.req.ready && !io.daddr.resp.fire()))
  when (io.flush.valid || (!io.daddr.req.fire() && io.dmem.req.fire())) {
    s1_valid := N
  } .elsewhen(!io.flush.valid && io.daddr.req.fire()) {
    s1_valid := Y
  }

  /* stage 2: send memory request */
  val mio_cycles = 2
  val s2_in = new LSUStage2Data(io.isu.bits, io.daddr.resp.bits.paddr)
  val s2_datas = Mem(mio_cycles, new LSUStage2Data)
  val s2_valids = RegInit(0.U(mio_cycles.W))
  val s2_blocking = s2_valids(0) && !io.wbu.fire()
  assert (!s2_blocking || !io.dmem.req.ready)
  io.dmem.req.valid := io.daddr.resp.valid
  io.dmem.req.bits.is_aligned := s2_in.op.isAligned()
  io.dmem.req.bits.addr  := Mux(s2_in.op.isAligned(),
    s2_in.addr, s2_in.addr & ~(3.U(32.W)))
  io.dmem.req.bits.func  := s2_in.op.func
  // L: 0 -> b1111, 1 -> b1110, 2 -> b1100, 3 -> b1000
  // R: 0 -> b0001, 1 -> b0011, 2 -> b0111, 3 -> b1111
  io.dmem.req.bits.wstrb := Mux(s2_in.op.isAligned(),
    0.U, Mux(s2_in.op.ext == LSU_L,
      "b1111000" >> (~s2_in.addr(0, 1)),
      "b0001111" >> (~s2_in.addr(0, 1))))
  io.dmem.req.bits.data := s2_in.data
  io.dmem.resp.ready := io.wbu.ready
  assert (!(s2_valids(0) && io.dmem.req.ready && !io.dmem.resp.fire()))
  when (io.flush.valid) {
    s2_valids := 0.U
  } .elsewhen (!s2_blocking) {
    s2_valids := Cat(io.dmem.req.fire(), s2_valids >> 1)
    for (i <- 0 until mio_cycles - 1) {
      s2_datas(i) := s2_datas(i + 1)
    }
    s2_datas(mio_cycles - 1) := s2_in
  }

  /* stage 3: recv contents */
  val s3_in = s2_datas(0)
  val s3_data = io.dmem.resp.bits.data
  io.wbu.valid := io.dmem.resp.valid
  io.wbu.bits.npc := pc
  io.wbu.bits.need_wb := Y
  io.wbu.bits.rd_idx := s3_in.rd_idx

  val lwstrb = "b1111000" >> (~s3_in.addr(0, 1))
  val lmask = Cat(for (i <- 0 until 4) yield l_mask(i).asTypeOf(SInt(8.W)))
  val rwstrb = "b0001111" >> (~s3_in.addr(0, 1))
  val rmask = Cat(for (i <- 0 until 4) yield l_mask(i).asTypeOf(SInt(8.W)))
  io.wbu.bits.data := Mux(s3_in.op.isAligned(),
    io.dmem.resp.bits.data, Mux(s3_in.op.ext == LSU_L,
      (lmask & s3_data) | (~lmask & s3_in.data),
      (rmask & s3_data) | (~rmask & s3_in.data)))
}

