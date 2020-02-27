package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.dumps._
import woop.utils._

class RegFile extends Module {
  val io = IO(new Bundle {
    val bp = Flipped(ValidIO(new BypassIO))
    val wb = Flipped(ValidIO(new WriteBackIO))
    val rfio = Flipped(new RegFileIO)
    val commit = Output(new CommitIO)
  })

  val wb_rf = Mem(32, UInt(conf.xprlen.W))
  val bp_rf = Mem(32, UInt(conf.xprlen.W))
  val rf_dirtys = Mem(32, Bool())
  val bp_readys = Mem(32, Bool())

  def bypass_match(idx:UInt) = io.bp.valid && io.bp.bits.rd_idx === idx
  def rf_data_ready(idx:UInt) = !rf_dirtys(idx) || bp_readys(idx) || bypass_match(idx) || idx === 0.U
  def rf_data_bits(idx:UInt) = MuxCase(0.U, Array(
    (idx === 0.U) -> 0.U,
    !rf_dirtys(idx) -> wb_rf(idx),
    bypass_match(idx) -> io.bp.bits.data,
    bp_readys(idx) -> bp_rf(idx)))

  io.rfio.rs_data.valid := rf_data_ready(io.rfio.rs_idx)
  io.rfio.rs_data.bits := rf_data_bits(io.rfio.rs_idx)

  io.rfio.rt_data.valid := rf_data_ready(io.rfio.rt_idx)
  io.rfio.rt_data.bits := rf_data_bits(io.rfio.rt_idx)

  when (io.rfio.wen) {
    rf_dirtys(io.rfio.rd_idx) := Y
    bp_readys(io.bp.bits.rd_idx) := N
  }

  when (io.wb.valid) {
    when (io.wb.bits.wen && io.wb.bits.rd_idx =/= 0.U) {
      wb_rf(io.wb.bits.rd_idx) := io.wb.bits.data
    }
    rf_dirtys(io.wb.bits.rd_idx) := N
  }

  when (io.bp.valid) {
    when (io.bp.bits.wen && io.bp.bits.rd_idx =/= 0.U) {
      bp_rf(io.bp.bits.rd_idx) := io.bp.bits.data
    }
    bp_readys(io.bp.bits.rd_idx) := Y
  }

  io.commit.valid := io.wb.valid
  io.commit.pc := io.wb.bits.pc
  io.commit.instr := io.wb.bits.instr.asUInt
  for (i <- 0 until 32) {
    io.commit.gpr(i) := Mux(io.wb.valid && io.wb.bits.wen && io.wb.bits.rd_idx === i.U, io.wb.bits.data, wb_rf(i))
  }

  if (conf.log_rf) {
    printf("%d: RF.commit: valid=%b, pc=%x, instr=%x\n", GTimer(), io.commit.valid, io.commit.pc, io.commit.instr)
    io.bp.dump("RF.bp")
    io.wb.dump("RF.wb")
    io.rfio.dump("RF.rfio")
    printf("%d: RF: rf_dirtys=%b\n", GTimer(), Cat(for (i <- 0 until 32) yield rf_dirtys(i)).asUInt)
    printf("%d: RF: bp_readys=%b\n", GTimer(), Cat(for (i <- 0 until 32) yield bp_readys(i)).asUInt)
    printf("%d: RF: RS@%d={bp_match:%b, rf_ready=%b, bits=%x, wbrf=%x, bprf=%x}\n", GTimer(), io.rfio.rs_idx, bypass_match(io.rfio.rs_idx), rf_data_ready(io.rfio.rs_idx), rf_data_bits(io.rfio.rs_idx), wb_rf(io.rfio.rs_idx), bp_rf(io.rfio.rs_idx))
    printf("%d: RF: RT@%d={bp_match:%b, rf_ready=%b, bits=%x, wbrf=%x, bprf=%x}\n", GTimer(), io.rfio.rt_idx, bypass_match(io.rfio.rt_idx), rf_data_ready(io.rfio.rt_idx), rf_data_bits(io.rfio.rt_idx), wb_rf(io.rfio.rt_idx), bp_rf(io.rfio.rt_idx))

    when (io.commit.valid) {
      printf("$pc:    %x\n", io.commit.pc)
      printf("$instr: %x\n", io.commit.instr)
      printf("$0 :%x $at:%x $v0:%x $v1:%x\n", io.commit.gpr(0), io.commit.gpr(1), io.commit.gpr(2), io.commit.gpr(3))
      printf("$a0:%x $a1:%x $a2:%x $a3:%x\n", io.commit.gpr(4), io.commit.gpr(5), io.commit.gpr(6), io.commit.gpr(7))
      printf("$t0:%x $t1:%x $t2:%x $t3:%x\n", io.commit.gpr(8), io.commit.gpr(9), io.commit.gpr(10), io.commit.gpr(11))
      printf("$t4:%x $t5:%x $t6:%x $t7:%x\n", io.commit.gpr(12), io.commit.gpr(13), io.commit.gpr(14), io.commit.gpr(15))
      printf("$s0:%x $s1:%x $s2:%x $s3:%x\n", io.commit.gpr(16), io.commit.gpr(17), io.commit.gpr(18), io.commit.gpr(19))
      printf("$s4:%x $s5:%x $s6:%x $s7:%x\n", io.commit.gpr(20), io.commit.gpr(21), io.commit.gpr(22), io.commit.gpr(23))
      printf("$t8:%x $t9:%x $k0:%x $k1:%x\n", io.commit.gpr(24), io.commit.gpr(25), io.commit.gpr(26), io.commit.gpr(27))
      printf("$gp:%x $sp:%x $fp:%x $ra:%x\n", io.commit.gpr(28), io.commit.gpr(29), io.commit.gpr(30), io.commit.gpr(31))
    }
  }
}
