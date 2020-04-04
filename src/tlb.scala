package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._


class TLBPhyn extends Bundle {
  val pfn = UInt(24.W)
  val c   = UInt(3.W)
  val d   = Bool()
  val v   = Bool()
}

class TLBEntry extends Bundle {
  val pagemask = UInt(16.W)
  val vpn  = UInt(19.W)
  val g    = Bool()
  val asid = UInt(8.W)
  val p0   = new TLBPhyn
  val p1   = new TLBPhyn
}

class MMURes extends Bundle {
  val ex = new CP0Exception
  val paddr = UInt(conf.xprlen.W)
}

class TLB extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(new TLBTransaction)
    val daddr = Flipped(new TLBTransaction)
    val rport = Flipped(new TLB_RPORT)
    val wport = Flipped(ValidIO(new TLB_WPORT))
    val pport = Flipped(new TLB_PPORT)
    val status = Input(new CP0Status)
    val br_flush = Flipped(ValidIO(new FlushIO))
    val ex_flush = Flipped(ValidIO(new FlushIO))
    val can_log_now = Input(Bool())
  })

  val tlb_entries = Mem(conf.tlbsz, new TLBEntry)
  val tlb_entry_ports = for (i <- 0 until conf.tlbsz) yield tlb_entries(i)

  def tlb_entry_match(vpn:UInt, tlb_port:TLBEntry) = {
    val mask = tlb_port.pagemask.asTypeOf(UInt(32.W))
    (tlb_port.vpn & ~mask) === (vpn & ~mask) &&
    (tlb_port.g || tlb_port.asid === io.pport.entry_hi.asid)
  }
  def tlb_entry_translate(vaddr:UInt, rwbit:UInt, tlb_port:TLBEntry) = {
    val mask = tlb_port.pagemask.asTypeOf(UInt(32.W))
    val eobit = (vaddr & ((mask + 1.U) << 12)).orR
    val phyn = Mux(eobit, tlb_port.p1, tlb_port.p0)
    val excode = Mux(rwbit === MX_RD, EC_TLBL, EC_TLBS)
    val res = WireInit(0.U.asTypeOf(new MMURes))
    when (phyn.v === 0.U) {
      res.ex.et := ET_TLB_Inv
      res.ex.code := excode
      res.paddr := 0.U
    } .elsewhen (phyn.d === 0.U && rwbit === MX_WR) {
      res.ex.et := ET_TLB_Mod
      res.ex.code := EC_Mod
      res.paddr := 0.U
    } .otherwise {
      val highbits = (phyn.pfn & ~mask) << 12
      val lowbits = vaddr & (((mask + 1.U) << 12) - 1.U)
      res.paddr := highbits | lowbits
      res.ex.et := ET_None
      res.ex.code := excode
    }
    res.ex.asid := tlb_port.asid
    res.ex.addr := vaddr
    res
  }

  def tlb_translate(vaddr:UInt, rwbit:UInt) = {
    val tlb_rports = for (i <- 0 until conf.tlbsz) yield tlb_entries(i)
    val matches = Reverse(Cat(for (i <- 0 until conf.tlbsz) yield
      tlb_entry_match(vaddr(31, 13), tlb_rports(i))))
    val miss = !matches.orR
    val hit_res = Mux1H(for (i <- 0 until conf.tlbsz) yield
      matches(i) -> tlb_entry_translate(vaddr, rwbit, tlb_rports(i)))
    val miss_res = WireInit(0.U.asTypeOf(new MMURes))
    miss_res.ex.et := ET_TLB_REFILL
    miss_res.ex.code := Mux(rwbit === MX_RD, EC_TLBL, EC_TLBS)
    miss_res.ex.addr := vaddr
    miss_res.paddr := 0.U
    Mux(miss, miss_res, hit_res)
  }

  def is_cached(vaddr:UInt) = vaddr(31, 29) =/= "b101".U
  def ioremap(addr:UInt) = Cat("b000".U, addr(28, 0))
  def vaddr2paddr(name:String, vaddr:UInt, rwbit:UInt) = {
    val mmu_res = tlb_translate(vaddr, rwbit)
    val vid = vaddr(31, 29)
    val paddr = Mux1H(Array(
      (vid === 4.U || vid === 5.U) -> ioremap(vaddr),
      (vid === 6.U) -> ioremap(mmu_res.paddr),
      (vid(1) === 0.U) -> Mux(io.status.ERL,
        vaddr, ioremap(mmu_res.paddr)),
    ))
    val et = Mux1H(Array(
      (vid === 4.U || vid === 5.U) -> ET_None,
      (vid === 6.U) -> mmu_res.ex.et,
      (vid(1) === 0.U) -> Mux(io.status.ERL, ET_None, mmu_res.ex.et),
    ))
    val res = WireInit(0.U.asTypeOf(new MMURes))
    res.ex.et := et
    res.ex.code := mmu_res.ex.code
    res.ex.addr := vaddr
    res.paddr := paddr

    if (conf.log_TLB) {
      printv(name+".v2p", Array[(String,Data)](
        ("mmu_res", mmu_res),
        ("vid", vid),
        ("paddr", paddr),
        ("et", et),
        ("res", res)))
    }

    res
  }

  def process_request(name:String, rio:TLBTransaction, flush:Bool) = {
    /* handle memory translate request, a pipeline stage */
    val tlbreq_in = RegEnable(rio.req.bits, enable=rio.req.fire())
    val tlbreq_valid = RegInit(N)
    val tlbreq_res = vaddr2paddr(name, tlbreq_in.vaddr, tlbreq_in.func)
    val addr_l2b = Mux(tlbreq_in.is_aligned,
      tlbreq_in.vaddr(1, 0), "b00".U(2.W))
    val addr_has_ex = Mux1H(Array(
      (tlbreq_in.len === ML_1) -> N,
      (tlbreq_in.len === ML_2) -> (addr_l2b(0)),
      (tlbreq_in.len === ML_4) -> (addr_l2b =/= 0.U),
    ))
    val addr_ex = WireInit(0.U.asTypeOf(new CP0Exception))
    addr_ex.et := ET_ADDR_ERR
    addr_ex.code := Mux(tlbreq_in.func === MX_RD, EC_AdEL, EC_AdES)

    rio.req.ready := rio.resp.ready || !tlbreq_valid
    rio.resp.valid := tlbreq_valid
    rio.resp.bits.paddr := tlbreq_res.paddr
    rio.resp.bits.is_cached := is_cached(tlbreq_in.vaddr)
    rio.resp.bits.ex := Mux(addr_has_ex, addr_ex, tlbreq_res.ex)

    when (flush || (!rio.req.fire() && rio.resp.fire())) {
      tlbreq_valid := N
    } .elsewhen (!flush && rio.req.fire()) {
      tlbreq_valid := Y
    }

    if (conf.log_TLB) {
      printv(name, Array[(String,Data)](
        ("tlbreq_valid", tlbreq_valid),
        ("tlbreq_in", tlbreq_in),
        ("tlbreq_res", tlbreq_res),
        ("addr_l2b", addr_l2b),
        ("addr_has_ex", addr_has_ex),
        ("addr_ex", addr_ex)))
    }
  }

  process_request("TLB.iaddr", io.iaddr, io.br_flush.valid || io.ex_flush.valid)
  process_request("TLB.daddr", io.daddr, io.ex_flush.valid)


  /* TLB rw io */
  io.rport.entry := Mux1H(
    for (i <- 0 until conf.tlbsz) yield
    (i.U === io.rport.index) -> tlb_entry_ports(i))
  when (io.wport.valid) {
    for (i <- 0 until conf.tlbsz) {
      when (i.U === io.wport.bits.index) {
        tlb_entry_ports(i) := io.wport.bits.entry
      }
    }
  }

  val matches = Reverse(Cat(for (i <- 0 until conf.tlbsz) yield tlb_entry_match(io.pport.entry_hi.vpn, tlb_entry_ports(i))))
  io.pport.index._0 := 0.U
  io.pport.index.p := !matches.orR
  io.pport.index.index := Mux1H(for (i <- 0 until conf.tlbsz) yield matches(i) -> i.U)

  if (conf.log_TLB) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printv(this, "TLB")
    printv(io.iaddr, "TLB.iaddr")
    printv(io.daddr, "TLB.daddr")
    printv(io.rport, "TLB.rport")
    printv(io.wport, "TLB.wport")
    printv(io.pport, "TLB.pport")
    printv(io.status, "TLB.status")
    printv(io.br_flush, "TLB.br_flush")
    printv(io.ex_flush, "TLB.ex_flush")
  }
}
