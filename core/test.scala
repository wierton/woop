package woopTest

import chisel3._
import chisel3.util._
import woop.configs._
import woop.consts._
import woop.utils._
import woop.core._
import woop._

class TestBitsOneWay extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
  })

  io.commit := DontCare

  printf("%d, %d\n", "b01".U.getWidth.U, 0x40.U.getWidth.U);
  printf("%x\n", BitsOneWay("b0".U(1.W)))
  printf("%x\n", BitsOneWay("b1".U(1.W)))
  printf("---\n");
  printf("%x\n", BitsOneWay("b01".U(2.W)))
  printf("%x\n", BitsOneWay("b10".U(2.W)))
  printf("%x\n", BitsOneWay("b11".U(2.W)))
  printf("---\n");
  printf("%x\n", BitsOneWay("b000".U(3.W)))
  printf("%x\n", BitsOneWay("b001".U(3.W)))
  printf("%x\n", BitsOneWay("b010".U(3.W)))
  printf("%x\n", BitsOneWay("b011".U(3.W)))
  printf("%x\n", BitsOneWay("b100".U(3.W)))
  printf("%x\n", BitsOneWay("b101".U(3.W)))
  printf("%x\n", BitsOneWay("b110".U(3.W)))
  printf("%x\n", BitsOneWay("b111".U(3.W)))
  printf("---\n");
}

class RespYourReq extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
  })

  io.in.req.ready := Y
  io.in.resp.valid := RegNext(io.in.req.valid)
  io.in.resp.bits.data := RegNext(io.in.req.bits.data)
  when (io.in.req.fire()) {
    printf("%d: resp %x\n", GTimer(), io.in.resp.bits.data);
  }
}

class TestMemCrossbar extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
  })

  io.commit := DontCare

  val resp_1 = new RespYourReq
  val as_1 = Array(new AddrSpace(0.U, 0x100.U))
  val crossbar_1 = new MemCrossbar(1, as_1)
  crossbar_1.io.out(0) <> resp_1.io.in

  when (GTimer() === 0.U) {
    crossbar_1.io.in(0).req.valid := Y
    crossbar_1.io.in(0).req.bits.data := 0xdeadbeef.U
  }

  when (GTimer() === 1.U) {
  }
}
