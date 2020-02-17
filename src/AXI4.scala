package MipsNPC

import chisel3._
import chisel3.util._


// write address channel
class AXI4_A(id_width:Int) extends Bundle {
    val id = Output(UInt(id_width.W));
    val addr = Output(UInt(32.W));  // address
    val len = Output(UInt(8.W)); // burst len, i + 1 transfers
    val size = Output(UInt(3.W)); // burst size, 2^i bytes
    val burst = Output(UInt(2.W)); // burst type, fixed, inc, dec
    val lock = Output(Bool());
    val cache = Output(UInt(4.W));
    val prot = Output(UInt(3.W));
    val region = Output(UInt(4.W));
    val qos = Output(UInt(4.W));
    val user = Output(UInt(5.W));

    override def cloneType = new AXI4_A(id_width).asInstanceOf[this.type];
}

class AXI4_AW(id_width:Int) extends AXI4_A(id_width) {
    override def cloneType = new AXI4_AW(id_width).asInstanceOf[this.type];
}

// write data channel
class AXI4_W(id_width:Int, data_width:Int) extends Bundle {
    val id = Output(UInt(id_width.W));
    val data = Output(UInt(data_width.W)); // write data
    val strb = Output(UInt((data_width / 8).W)); // mask
    val last = Output(Bool());
    val user = Output(UInt(5.W));

    override def cloneType = new AXI4_W(id_width, data_width).asInstanceOf[this.type];
}

// write response channel
class AXI4_B(id_width:Int) extends Bundle {
    val id = Input(UInt(id_width.W));
    val resp = Input(UInt(2.W));
    val user = Output(UInt(5.W));

    override def cloneType = new AXI4_B(id_width).asInstanceOf[this.type];
}

// read address channel
class AXI4_AR(id_width:Int) extends AXI4_A(id_width) {
    override def cloneType = new AXI4_AR(id_width).asInstanceOf[this.type];
}

// read data channel
class AXI4_R(id_width:Int, data_width:Int) extends Bundle {
    val id = Input(UInt(id_width.W));
    val data = Input(UInt(data_width.W));
    val resp = Input(UInt(2.W));
    val last = Input(Bool());
    val user = Output(UInt(5.W));

    override def cloneType = new AXI4_R(id_width, data_width).asInstanceOf[this.type];
}

class AXI4IO(id_width:Int, data_width:Int) extends Bundle {
    // address write
    val aw = DecoupledIO(new AXI4_AW(id_width));
    // data write
    val w = DecoupledIO(new AXI4_W(id_width, data_width));
    // write response
    val b = DecoupledIO(new AXI4_B(id_width));
    // address read
    val ar = DecoupledIO(new AXI4_AR(id_width));
    // read response
    val r = DecoupledIO(new AXI4_R(id_width, data_width));

    override def cloneType = new AXI4IO(id_width, data_width).asInstanceOf[this.type];
}
