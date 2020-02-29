package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._

object LSUConsts extends LSUConsts { }

class LSUOp extends Bundle {
  val align = Bool()
  val func  = UInt(1.W)
  val len   = UInt(2.W)
  val ext   = UInt(1.W)

  // below functions only used for unaligned rw
  // LWL: A strb   mem[A]        Reg           RegF
  //      0 0001 0xAABBCCDD | 0x11223344 -> 0xDD223344
  //      1 0011 0xAABBCCDD | 0x11223344 -> 0xCCDD3344
  //      2 0111 0xAABBCCDD | 0x11223344 -> 0xBBCCDD44
  //      3 1111 0xAABBCCDD | 0x11223344 -> 0xAABBCCDD
  // LWR: A strb   mem[A]        Reg           RegF
  //      0 1111 0xAABBCCDD | 0x11223344 -> 0xAABBCCDD
  //      1 1110 0xAABBCCDD | 0x11223344 -> 0x11AABBCC
  //      2 1100 0xAABBCCDD | 0x11223344 -> 0x1122AABB
  //      3 1000 0xAABBCCDD | 0x11223344 -> 0x112233AA
  // SWL: A strb   mem[A]        Reg           MemF
  //      0 0001 0xAABBCCDD | 0x11223344 -> 0xAABBCC11
  //      1 0011 0xAABBCCDD | 0x11223344 -> 0xAABB1122
  //      2 0111 0xAABBCCDD | 0x11223344 -> 0xAA112233
  //      3 1111 0xAABBCCDD | 0x11223344 -> 0x11223344
  // SWR: A strb   mem[A]        Reg           MemF
  //      0 1111 0xAABBCCDD | 0x11223344 -> 0x11223344
  //      1 1110 0xAABBCCDD | 0x11223344 -> 0x223344DD
  //      2 1100 0xAABBCCDD | 0x11223344 -> 0x3344CCDD
  //      3 1000 0xAABBCCDD | 0x11223344 -> 0x44BBCCDD
  def strbOf(addr:UInt) = Mux(align,
    "b0001111".U >> (~len),
    Mux(ext === LSUConsts.LSU_L,
      "b0001111".U >> (~addr(1, 0)),
      "b1111000".U >> (~addr(1, 0)))) (3, 0)
  def maskOf(addr:UInt) = {
    val strb = strbOf(addr)
    Reverse(Cat(for (i <- 0 until 4) yield strb(i).asTypeOf(SInt(8.W))))
  }
  def dataOf(addr:UInt, data:UInt, mask_data:UInt) = {
    val l2b = addr(1, 0)
    val mask = maskOf(addr)
    val rdata = Mux(align, data, Mux(ext === LSUConsts.LSU_L, data << ((~l2b) << 3), data >> (l2b << 3)))
    val rmask = Mux(align, mask, Mux(ext === LSUConsts.LSU_L, mask << ((~l2b) << 3), mask >> (l2b << 3)))
    if (conf.log_LSU) {
      when (TraceTrigger()) { dump() }
    }

    def dump():Unit = {
        printf("%d: LSU.c: addr=%x, data=%x, mdata=%x, mask=%x, rdata=%x, rmask=%x\n", GTimer(), addr, data, mask_data, mask, rdata, rmask)
      }
    (rdata & rmask) | (mask_data & ~rmask)
  }
  def memReqDataOf(addr:UInt, data:UInt) = {
    val l2b = addr(1, 0)
    Mux(align, data,
      Mux(ext === LSUConsts.LSU_L,
        data >> (~l2b << 3),
        data << (l2b << 3)))
  }
}

object AddrLen2Strb {
  def apply(addr:UInt, len:UInt) = {
    val l2 = addr(1, 0)
    val h2 = l2 + len
    assert (h2 < 4.U)
    Reverse(Cat(for (i <- 0 until 4) yield l2 <= i.U && i.U <= h2))
  }
}

class LSUStage2Data extends Bundle {
  val op = new LSUOp
  val instr = new Instr
  val rd_idx = UInt(REG_SZ.W)
  val pc = UInt(conf.xprlen.W)
  val id = UInt(conf.INSTR_ID_SZ.W)
  val data = UInt(conf.xprlen.W)
  val addr = UInt(conf.xprlen.W)
  val is_cached = Bool()

  def load(ind:PRALU_LSMDU_IO) = {
    this := Cat(
      ind.ops.fu_op,
      ind.wb.instr.asUInt,
      ind.wb.rd_idx,
      ind.wb.pc,
      ind.wb.id,
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
  io.dmem.req.bits.is_cached  := s2_in.is_cached
  io.dmem.req.bits.is_aligned := s2_in.op.align
  io.dmem.req.bits.addr  := Mux(s2_in.op.align, s2_in.addr, s2_in.addr & ~(3.U(32.W)))
  io.dmem.req.bits.len   := s2_in.op.len
  io.dmem.req.bits.func  := s2_in.op.func
  io.dmem.req.bits.data  := s2_in.op.memReqDataOf(s2_in.addr, s2_in.data)
  io.dmem.req.bits.strb  := s2_in.op.strbOf(s2_in.addr)
  io.dmem.resp.ready := Y

  /* stage 3: recv contents and commit */
  val s3_in = s2_datas.io.deq.bits
  val ze_data = s3_in.op.dataOf(s3_in.addr,
    io.dmem.resp.bits.data, Mux(s3_in.op.align,
      0.U(32.W), s3_in.data))
  /* io.fu_out.bits.wb.data */
  io.fu_out.valid := io.dmem.resp.valid
  io.fu_out.bits.v := s3_in.op.func === MX_RD
  io.fu_out.bits.pc := s3_in.pc
  io.fu_out.bits.id := s3_in.id
  io.fu_out.bits.wen := s3_in.op.func === MX_RD
  io.fu_out.bits.rd_idx := s3_in.rd_idx
  io.fu_out.bits.instr := s3_in.instr
  io.fu_out.bits.data := MuxCase(ze_data, Array(
    (s3_in.op.asUInt === LSU_LB) -> io.dmem.resp.bits.data(7, 0).asTypeOf(SInt(32.W)).asUInt,
    (s3_in.op.asUInt === LSU_LH) -> io.dmem.resp.bits.data(15, 0).asTypeOf(SInt(32.W)).asUInt))

  if (conf.log_LSU) {
    when (TraceTrigger()) { dump() }
  }

  def dump():Unit = {
    s2_in.dump("LSU.s2_in")
    s3_in.dump("LSU.s3_in")
    s2_datas.io.enq.dump("LSU.s2_datas.enq")
    s2_datas.io.deq.dump("LSU.s2_datas.deq")
    io.fu_in.dump("LSU.io.in")
    io.fu_out.dump("LSU.io.out")
    io.dmem.dump("LSU.io.dmem")
  }
}

