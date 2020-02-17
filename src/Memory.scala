package MipsNPC

import chisel3._
import chisel3.util._

import Configure._
import ModuleConsts._
import ModuleIO._

object MemConsts {
	// DT
	val DT_SZ = 2;
	val DT_X  = 0.U(DT_SZ.W);
	val DT_B  = 0.U(DT_SZ.W);
	val DT_H  = 1.U(DT_SZ.W);
    val DT_T  = 2.U(DT_SZ.W);
	val DT_W  = 3.U(DT_SZ.W);

	// MX
	val MX_SZ = 1;
	val MX_X  = 0.U(MX_SZ.W);
	val MX_RD = 0.U(MX_SZ.W);
	val MX_WR = 1.U(MX_SZ.W);

    val MEM_ST = 0x10000000;
    val MEM_ED = 0x18000000;
}

import MemConsts._

class MemoryRequest extends Bundle {
	val dtype = Output(UInt(DT_SZ.W)); // enable s
	val func  = Output(UInt(MX_SZ.W));
	val data  = Output(UInt(conf.xprlen.W));
	val addr  = Output(UInt(conf.xprlen.W));
}

class MemoryResponse extends Bundle {
	val data = Output(UInt(conf.xprlen.W));
}

class MemoryIO extends Bundle {
	val req = DecoupledIO(new MemoryRequest);
	val resp = Flipped(DecoupledIO(new MemoryResponse));
}

trait MemoryMapped {
    def io_in = Flipped(new MemoryIO);
    val start_addr:Int = 0;
    val end_addr:Int = 0;
}

class AsyncMemoryArbiter(n:Int, dMemIdx:Int) extends Module with MemoryMapped {
	val io = IO(new Bundle {
        val imem = Flipped(new MemoryIO);
        val dmem = Flipped(new MemoryIO);
		val out = new AXI4IO(conf.axi_id_width, conf.axi_data_width);
	});

    override def io_in = io.in(dMemIdx);
    override val start_addr = MEM_ST;
    override val end_addr   = MEM_ED;

    io.out.ar.valid := false.B;
    io.out.ar.bits.burst := 1.U;
    io.out.ar.bits.lock := false.B; io.out.ar.bits.cache := 0.U;
    io.out.ar.bits.prot := 0.U; io.out.ar.bits.region := 0.U;
    io.out.ar.bits.qos := 0.U; io.out.ar.bits.user := 0.U;

    io.out.aw.valid := false.B;
    io.out.aw.bits.id := 0.U;
    io.out.aw.bits.addr := 0.U;
    io.out.aw.bits.len := 0.U;
    io.out.aw.bits.size := 0.U;
    io.out.aw.bits.burst := 1.U;
    io.out.aw.bits.lock := false.B; io.out.aw.bits.cache := 0.U;
    io.out.aw.bits.prot := 0.U; io.out.aw.bits.region := 0.U;
    io.out.aw.bits.qos := 0.U; io.out.aw.bits.user := 0.U;

    io.out.w.valid := false.B;
    io.out.w.bits.id := 0.U;
    io.out.w.bits.data := 0.U;
    io.out.w.bits.strb := 0.U;
    io.out.w.bits.last := false.B;

    io.out.r.ready := true.B;
    io.out.b.ready := true.B;

    // IF
    val imem_valid = io.imem.req.valid;
    val dmem_valid = io.dmem.req.valid;
    val dmem_read = dmem_valid && io.dmem.req.bits.func === MX_RD;
    val dmem_write = dmem_valid && io.dmem.req.bits.func === MX_WR;

    io.imem.req.ready := io.out.ar.ready;

    io.out.ar.valid := Mux(imem_valid, imem_valid, dmem_valid);
    io.out.ar.bits.id := Mux(imem_valid, imem_valid, dmem_valid);
    io.out.ar.bits.addr := Mux(imem_valid, imem_addr, dmem_addr);
    io.out.ar.bits.len := 0.U; // no burst
    io.out.ar.bits.size := Mux(imem_valid, imem_len, dmem_len);

    // INSTR
    io.imem.resp.valid := io.out.r.valid;
    io.imem.resp.data := io.out.r.data;


    io.dmem.req.ready := Mux(dmem_write, io.out.aw.ready, Mux(
        io.imem.req.valid, false.B, io.out.ar.ready
    ));
}

class AsyncCrossbar(modules:MemoryMapped*) extends Module {
    val n = modules.length;
    val io = IO(new Bundle {
        val in = Flipped(new MemoryIO);
        val out = Vec(n, new MemoryIO);
        val gpio = new CROSSBAR_GPIO_IO;
    });

    def checkCross(modules:MemoryMapped*) {
        for(m <- modules) {
            // print some logs
            println("[CROSSBAR] (0x" + m.start_addr.toHexString + ",0x" + m.end_addr.toHexString + ")");
        }

        for(i <- 0 until n) {
            for(j <- i + 1 until n) {
                assert(!( (modules(i).start_addr < modules(j).start_addr
                && modules(j).start_addr <= modules(i).end_addr)
                || (modules(i).start_addr < modules(j).end_addr
                && modules(j).end_addr <= modules(i).end_addr) ));
            }
        }
    }

    checkCross(modules:_*);

    io.in.req.ready := true.B;
    io.in.resp.valid := false.B;
    io.in.resp.bits.data := 0.U;

    val valid = Cat(for(i <- n-1 to 0 by -1) yield modules(i).start_addr.U <= io.in.req.bits.addr && io.in.req.bits.addr < modules(i).end_addr.U);

    for(i <- 0 until n) {
        when(io.in.req.valid && valid(i)) {
            log("[CROSSBAR@%x] address %x, valid:%x\n", i.U, io.in.req.bits.addr, valid);
        }
    }

    for(i <- 0 until n) {
        io.out(i).req.bits.func  <> io.in.req.bits.func;
        io.out(i).req.bits.dtype <> io.in.req.bits.dtype;
        io.out(i).req.bits.addr  <> io.in.req.bits.addr - modules(i).start_addr.U;
        io.out(i).req.bits.data  <> io.in.req.bits.data;
        io.out(i).req.valid := false.B;
        io.out(i).resp.ready := io.in.resp.ready;

        when(valid(i)) {
            io.out(i).req.valid := io.in.req.valid;
        }

        when(io.out(i).resp.valid) {
            io.in.resp <> io.out(i).resp;
        }
    }

    // GPIO
    io.gpio.invalid_address := false.B;
    io.gpio.addr := 0.U;
    when(io.in.req.valid && !valid.orR) {
        io.gpio.invalid_address := true.B;
        io.gpio.addr := io.in.req.bits.addr;
        log("[CROSSBAR] Invalid address '%x'\n", io.in.req.bits.addr);
    }

    def connect():AsyncCrossbar = {
        for(i <- 0 until n) {
            io.out(i) <> modules(i).io_in;
        }
        return this;
    }
}

class SimulatedMemory(idwidth:Int) extends BlackBox {
	val io = IO(new Bundle {
		val clock = Input(Clock());
		val reset = Input(Bool());
		val mem = Flipped(new SimMemoryIO(idwidth));
	});
}

trait GPIOConstants {
    // val halt_idx = ;
}

class TestMappedIO extends Module with MemoryMapped {
    val io = IO(new Bundle {
        val mem = Flipped(new MemoryIO);
    });

    override def io_in = io.mem;
    override val start_addr = 0x50001000;
    override val end_addr   = 0x50001004;

    when(io.mem.req.valid) {
        log("[TESTIO] func:%x,dtype:%x,addr:%x\n", io.mem.req.bits.func, io.mem.req.bits.dtype, io.mem.req.bits.addr);
    }

    io.mem.req.ready := true.B;
    io.mem.resp.valid := RegNext(io.mem.req.valid);
    io.mem.resp.bits.data := 0.U;
}

class SimulatedSerial extends BlackBox with MemoryMapped {
	val io = IO(new Bundle {
        val clock = Input(Clock());
        val reset = Input(Bool());
		val mem = Flipped(new MemoryIO);
	});

    val cnt = 4;

    override def io_in = io.mem;
    override val start_addr = 0x40001000;
    override val end_addr   = start_addr + cnt * conf.xprbyte;
}

class GPIO extends Module with MemoryMapped {
    val io = IO(new Bundle {
        val in = Flipped(new MemoryIO); // from crossbar
        val idu = Flipped(new IDU_GPIO_IO);
        val crossbar = Flipped(new CROSSBAR_GPIO_IO);
        val out = new GPIO_OUT;
    });

    val cnt  = 32;
    override def io_in = io.in;
    override val start_addr = 0x40000000;
    override val end_addr   = start_addr + cnt * conf.xprbyte;

    val halted = RegInit(false.B);
    val reason = RegInit(0.U);
    val status = Mem(cnt, UInt(conf.xprlen.W));

    io.in.req.ready := true.B;
    io.in.resp.bits.data := 0.U;
    io.in.resp.valid := io.in.req.valid;
    io.out.code := status(0);
    io.out.halted := halted;
    io.out.reason := reason;

    // halted state
    io.idu.halted := halted;

    when(io.idu.invalid_instr) {
        log("[GPIO] io.idu.invalid_instr\n");
        halted := true.B;
        reason := io.idu.npc;
        status(0) := HALT_INVALID_INSTR.U;
    }

    when(io.crossbar.invalid_address) {
        log("[GPIO] io.crossbar.invalid_address\n");
        halted := true.B;
        reason := io.crossbar.addr;
        status(0) := HALT_INVALID_MEM_REQ.U;
    }

    when(io.in.req.valid) {
        val sel = io.in.req.bits.addr(log2Up(cnt) + 2, 2);
        log("[GPIO] addr:%x, sel:%x, data:%x\n", io.in.req.bits.addr, sel, io.in.req.bits.data);
        when(io.in.req.bits.func === MX_RD) {
            io.in.resp.bits.data := status(sel);
        }

        when(io.in.req.bits.func === MX_WR) {
            when(sel === 0.U) {
                reason := 0xF003.U;
                halted := true.B;
            }
            status(sel) := io.in.req.bits.data;
        }
    }
}

class SimAXI4Memory(id_width:Int, data_width:Int) extends Module {
    val io = IO(Flipped(new AXI4IO(id_width, data_width)));
    val mem_size = 128 * 1024 * 1024;
    val mem = Mem(mem_size, UInt(8.W));

    /////////////////////////////////////////////////////////////
    //////////////////          Read        /////////////////////
    /////////////////////////////////////////////////////////////
    val ar = RegInit(io.ar.bits);
    val ar_valid = RegEnable(next=true.B, enable=io.ar.fire(), init=false.B);
    val ar_data = RegInit(0.U(data_width.W));
    val ar_last = RegInit(true.B);

    io.ar.ready := ar_last;

    io.r.valid := ar_valid;
    io.r.bits.id := ar.id;
    io.r.bits.data := ar_data;
    io.r.bits.resp := 0.U;
    io.r.bits.last := ar_last;
    io.r.bits.user := 0.U;

    assert(io.ar.bits.burst === 1.U);

    private def read(in:AXI4_AR) {
        val burst_bytes = 1.U(1.W) << in.size;
        assert((burst_bytes << 3) <= data_width.U);
        printf("READ: r_burst_bytes:%x\n", burst_bytes);

        // bytes
        val r_data = Cat(for(i <- (data_width / 8 - 1) to 0 by -1) yield Mux(i.U < burst_bytes, mem(in.addr + i.U), 0.U(8.W))).asUInt;

        printf("READ: addr:%x, size:%x, len:%x, data:%x\n", in.addr, in.size, in.len, r_data);

        ar_data := r_data;
        ar.addr := in.addr + burst_bytes;
        ar.size := in.size;
        ar.len := in.len - 1.U;
        ar.id := in.id;
        ar_last := in.len === 0.U;
    }

    // cycle when fire, prepare data
    when(io.ar.fire()) { read(io.ar.bits); }

    // cycle after fire, prepare burst data
    //   if next_cycle_burst_read is true, the ar can't fire
    when(!ar_last && io.r.fire()) { read(ar); }

    // last burst cycle
    when(ar_last && !io.ar.fire()) {
        ar_valid := false.B;
    }



    /////////////////////////////////////////////////////////////
    //////////////////         Write        /////////////////////
    /////////////////////////////////////////////////////////////
    val aw = RegInit(io.aw.bits);
    val b_valid = RegInit(false.B);
    val aw_burst = RegInit(false.B);
    val aw_data = RegInit(0.U(data_width.W));
    val w_last = io.w.fire() && io.w.bits.last;

    io.aw.ready := !aw_burst;
    io.w.ready := aw_burst || io.aw.fire();

    io.b.valid := b_valid;
    io.b.bits.id := aw.id;
    io.b.bits.resp := 0.U;
    io.b.bits.user := 0.U;

    when(w_last) { b_valid := true.B; }
    when(io.b.fire() && !w_last) { b_valid := false.B; }

    printf("aw_burst:%x, w_last:%x\n", aw_burst, w_last);

    private def write(in:AXI4_AW, d:AXI4_W) {
        val burst_bytes = (1.U(1.W) << in.size);
        assert((burst_bytes << 3) < data_width.U);
        assert(in.burst === 1.U);
        // update registers
        aw.id := in.id;
        aw.addr := in.addr + burst_bytes;
        // actual write operation
        for(i <- 0 until data_width / 8) {
            when(i.U < burst_bytes && d.strb(i) === 1.U) {
                printf("write(%x, %x)\n", in.addr + i.U, d.data((i+1)*8-1, i*8));
                mem(in.addr + i.U) := d.data((i+1)*8-1, i*8);
            }
        }
        aw_burst := !d.last;
    }

    // the first only-aw cycle
    when(io.aw.fire() && !io.w.fire()) { aw := io.aw.bits; }
    // first with-aw write
    when(io.aw.fire() && io.w.fire()) { write(io.aw.bits, io.w.bits); }
    // remained write
    when(aw_burst && io.w.fire()) { write(aw, io.w.bits); }
}

