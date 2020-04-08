package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._


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

class LSU extends Module with LSUConsts {
  val io = IO(new Bundle {
    val dmem = new MemIO
    val fu_in = Flipped(DecoupledIO(new EHU_LSMDU_IO))
    val fu_out = ValidIO(new WriteBackIO)
    val working = Output(Bool())
    val can_log_now = Input(Bool())
  })

  io.fu_in.ready := !io.fu_in.valid || io.dmem.req.ready

  /* stage 2: send memory request */
  val s2_in = io.fu_in.bits
  val s2_in_op = s2_in.ops.fu_op.asTypeOf(new LSUOp)
  val s2_datas = Module(new Queue(new EHU_LSMDU_IO, conf.mio_cycles))
  s2_datas.io.enq.valid := io.dmem.req.fire()
  s2_datas.io.enq.bits := s2_in
  s2_datas.io.deq.ready := io.dmem.resp.fire()
  io.working := s2_datas.io.deq.valid
  io.dmem.req.valid := io.fu_in.valid
  io.dmem.req.bits.is_cached  := s2_in.is_cached
  io.dmem.req.bits.is_aligned := s2_in_op.align
  io.dmem.req.bits.addr  := Mux(s2_in_op.align,
    s2_in.ops.op1, s2_in.ops.op1 & ~(3.U(32.W)))
  io.dmem.req.bits.len   := s2_in_op.len
  io.dmem.req.bits.func  := s2_in_op.func
  io.dmem.req.bits.data  := s2_in_op.memReqDataOf(s2_in.ops.op1, s2_in.ops.op2)
  io.dmem.req.bits.strb  := s2_in_op.strbOf(s2_in.ops.op1)
  io.dmem.resp.ready := Y

  /* stage 3: recv contents and commit */
  val s3_in = s2_datas.io.deq.bits
  val s3_in_op = s3_in.ops.fu_op.asTypeOf(new LSUOp)
  val ze_data = s3_in_op.dataOf(s3_in.ops.op1,
    io.dmem.resp.bits.data, Mux(s3_in_op.align,
      0.U(32.W), s3_in.ops.op2))
  /* io.fu_out.bits.wb.data */
  io.fu_out.valid := io.dmem.resp.valid
  io.fu_out.bits.v := s3_in_op.func === MX_RD || s3_in_op.asUInt === LSU_SC
  io.fu_out.bits.pc := s3_in.wb.pc
  io.fu_out.bits.id := s3_in.wb.id
  io.fu_out.bits.wen := s3_in_op.func === MX_RD || s3_in_op.asUInt === LSU_SC
  io.fu_out.bits.rd_idx := s3_in.wb.rd_idx
  io.fu_out.bits.instr := s3_in.wb.instr
  io.fu_out.bits.is_ds := N
  io.fu_out.bits.ip7 := s3_in.wb.ip7
  io.fu_out.bits.is_br := s3_in.wb.is_br
  io.fu_out.bits.npc := s3_in.wb.npc
  io.fu_out.bits.data := MuxCase(ze_data, Array(
    (s3_in_op.asUInt === LSU_SC) -> 1.U,
    (s3_in_op.asUInt === LSU_LB) -> io.dmem.resp.bits.data(7, 0).asTypeOf(SInt(32.W)).asUInt,
    (s3_in_op.asUInt === LSU_LH) -> io.dmem.resp.bits.data(15, 0).asTypeOf(SInt(32.W)).asUInt))

  if (conf.log_LSU) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "LSU")
    printv(s2_in, "LSU.s2_in")
    printv(s3_in, "LSU.s3_in")
    printv(s2_datas.io.enq, "LSU.s2_datas.enq")
    printv(s2_datas.io.deq, "LSU.s2_datas.deq")
    printv(io.fu_in, "LSU.fu_in")
    printv(io.fu_out, "LSU.fu_out")
    printv(io.dmem, "LSU.dmem")
  }
}

