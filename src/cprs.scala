package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._

class CP0Status extends Bundle {
  val CU   = UInt(4.W)
  val RP   = UInt(1.W)
  val FR   = UInt(1.W)
  val RE   = UInt(1.W)
  val MX   = UInt(1.W)
  
  val PX   = UInt(1.W)
  val BEV  = UInt(1.W)
  
  val TS   = UInt(1.W)
  val SR   = UInt(1.W)
  val NMI  = UInt(1.W)
  val _0   = UInt(1.W)
  val Impl = UInt(2.W)
  
  val IM   = Vec(8, Bool())
  
  val KX   = UInt(1.W)
  val SX   = UInt(1.W)
  val UX   = UInt(1.W)
  val UM   = UInt(1.W) // --\
                       //   +--> KSU
  val R0   = UInt(1.W) // --/
  val ERL  = Bool()
  val EXL  = Bool()  // 0 for user, 1 for kernel
  val IE   = Bool()  // 0 for disable, 1 for enable

  def init(): Unit = {
    require(this.getWidth == 32)
    this.CU := 1.U
    this.RP := 0.U
    this.FR := 0.U
    this.RE := 0.U
    this.MX := 0.U

    this.PX := 0.U
    this.BEV := 1.U

    this.TS := 0.U
    this.SR := 0.U
    this.NMI := 0.U
    this._0 := 0.U
    this.Impl := 0.U

    this.IM := 0.U.asTypeOf(this.IM)

    this.KX := 0.U
    this.SX := 0.U
    this.UX := 0.U
    this.UM := 0.U

    this.R0 := 0.U
    this.ERL := 0.U
    this.EXL := 0.U
    this.IE := 0.U
  }

  def write(value: UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.CU := newVal.CU
    this.RP := newVal.RP
    this.RE := newVal.RE
    this.BEV := newVal.BEV
    this.TS := newVal.TS
    this.SR := newVal.SR
    this.NMI := newVal.NMI
    this.IM := newVal.IM
    this.UM := newVal.UM
    this.ERL := newVal.ERL
    this.EXL := newVal.EXL
    this.IE := newVal.IE
  }
}

class CP0Cause extends Bundle {
  val BD = UInt(1.W)
  val _1 = UInt(1.W)
  val CE = UInt(2.W)
  val _2 = UInt(4.W)
  
  val IV = UInt(1.W)
  val WP = UInt(1.W)
  val _3 = UInt(6.W)
  
  val IP = Vec(8, Bool())
  val _4 = UInt(1.W)
  val ExcCode = UInt(5.W)
  val _5 = UInt(2.W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value: UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.IV := newVal.IV
    this.WP := newVal.WP
    this.IP(0) := newVal.IP(0)
    this.IP(1) := newVal.IP(1)
  }
}

class CP0Wired extends Bundle {
  val _0  = UInt((32 - log2Ceil(conf.tlbsz)).W)
  val bound = UInt(log2Ceil(conf.tlbsz).W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value: UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.bound := newVal.bound
  }
}

class CP0Context extends Bundle {
  val ptebase = UInt(9.W)
  val badvpn2 = UInt(19.W)
  val _0 = UInt(4.W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value: UInt): Unit = {
    // maintained by kernel
    val newVal = value.asTypeOf(this)
    this.ptebase := newVal.ptebase
  }
}

class CP0Prid extends Bundle {
  val company_options = UInt(8.W)
  val company_id      = UInt(8.W)
  val processor_id    = UInt(8.W)
  val revision        = UInt(8.W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this.company_options := 0.U
    this.company_id := 0x01.U
    this.processor_id := 0x80.U  // mips32 4Kc
    this.revision := 0.U
  }

  def write(value: UInt): Unit = {
  }
}

class CP0Config extends Bundle {
  val M    = UInt(1.W) // donate that config1 impled at sel 1
  val Impl = UInt(15.W)
  val BE   = UInt(1.W) // 0 for little endian, 1 for big endian
					   // 3 reserved
                       // 2 for mips64 with all access to 32-bit seg
                       // 1 for mips64 with access only to 32-bit seg
  val AT   = UInt(2.W) // 0 for mips32,
  val AR   = UInt(3.W) // 0 for revision 1
					   // 2 xxx, 3 xxx
					   // 1 for standard TLB
                       // 0 for none
  val MT   = UInt(3.W) // MMU type
  val _0   = UInt(4.W) // must be zero
  val K0   = UInt(3.W) // kseg0 coherency algorithms

  def init(): Unit = {
    require(this.getWidth == 32)
    this.K0 := 0.U
    this._0 := 0.U
    this.MT := 1.U
    this.AR := 0.U
    this.AT := 0.U
    this.BE := 0.U
    this.Impl := 0.U
    this.M := 1.U
  }

  def write(value: UInt): Unit = {
  }
}

class CP0Config1 extends Bundle {
  val M  = UInt(1.W) // indicate config 2 is present
  val MMU_size = UInt(6.W) // 0 to 63 indicates 1 to 64 TLB entries
				   // ---------------------------
                   // 2^(IS + 8)
  val IS = UInt(3.W) // icache sets per way:
				   // ---------------------------
				   // othwise: 2^(IL + 1) bytes
                   // 0 for no icache, 7 reserved
  val IL = UInt(3.W) // icache line size: 
				   // ---------------------------
				   // 2^(IA) ways
                   // 0 for direct mapped
  val IA = UInt(3.W) // icache associativity
				   // ---------------------------
                   // 2^(IS + 8)
  val DS = UInt(3.W) // dcache sets per way:
				   // ---------------------------
				   // othwise: 2^(DL + 1) bytes
                   // 0 for no icache, 7 reserved
  val DL = UInt(3.W) // dcache line size: 
				   // ---------------------------
				   // 2^(DA) ways
                   // 0 for direct mapped
  val DA = UInt(3.W) // dcache associativity
  
  val C2 = UInt(1.W) // coprocessor present bit
  val MD = UInt(1.W) // not used on mips32 processor
  val PC = UInt(1.W) // performance counter present bit

  val WR = UInt(1.W) // watch registers present bit
  val CA = UInt(1.W) // code compression present bit
  val EP = UInt(1.W) // EJTAG present bit
  val FP = UInt(1.W) // FPU present bit

  def init(): Unit = {
    require(this.getWidth == 32)
    this.FP := 0.U
    this.EP := 0.U
    this.CA := 0.U
    this.WR := 0.U

    this.PC := 0.U
    this.MD := 0.U
    this.C2 := 0.U

    require(conf.nICacheSets >= 1)
    require(log2Ceil(conf.nICacheWayBytes) >= 1)
    require(log2Ceil(conf.nICacheSets) >= 6)
    this.IA := (conf.nICacheWays - 1).U
    this.IL := (log2Ceil(conf.nICacheWayBytes) - 1).U
    this.IS := (log2Ceil(conf.nICacheSets) - 6).U

    require(conf.nDCacheSets >= 1)
    require(log2Ceil(conf.nDCacheWayBytes) >= 1)
    require(log2Ceil(conf.nDCacheSets) >= 6)
    this.DA := (conf.nDCacheWays - 1).U
    this.DL := (log2Ceil(conf.nDCacheWayBytes) - 1).U
    this.DS := (log2Ceil(conf.nDCacheSets) - 6).U

    require(conf.tlbsz <= 64)
    this.MMU_size := (conf.tlbsz - 1).U
    this.M := 0.U
  }

  def write(value: UInt): Unit = {
  }
}

class CP0EntryLO extends Bundle {
  val _0  = UInt(2.W)
  val _1  = UInt((36 - conf.PABITS).W)
  val pfn = UInt((conf.PABITS - 12).W)
  val c   = UInt(3.W)
  val d   = Bool()
  val v   = Bool()
  val g   = Bool()

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value:UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.pfn := newVal.pfn
    this.c := newVal.c
    this.d := newVal.d
    this.v := newVal.v
    this.g := newVal.g
  }
}

class CP0EntryHI extends Bundle {
  val vpn = UInt(19.W)
  val _0   = UInt(5.W)
  val asid = UInt(8.W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value:UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.vpn := newVal.vpn
    this.asid := newVal.asid
  }
}

class CP0PageMask extends Bundle {
  val _0   = UInt(3.W)
  val mask = UInt(16.W)
  val _1   = UInt(13.W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value:UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.mask := newVal.mask
  }
}

class CP0Random extends Bundle {
  val _0    = UInt((32 - log2Ceil(conf.tlbsz)).W)
  val index = UInt((log2Ceil(conf.tlbsz)).W)

  def init(): Unit = {
    require(this.getWidth == 32)
  }

  def write(value:UInt): Unit = {
  }
}

class CP0Index extends Bundle {
  val p     = UInt(1.W)
  val _0    = UInt((31 - log2Ceil(conf.tlbsz)).W)
  val index = UInt((log2Ceil(conf.tlbsz)).W)

  def init(): Unit = {
    require(this.getWidth == 32)
    this := 0.U.asTypeOf(this)
  }

  def write(value:UInt): Unit = {
    val newVal = value.asTypeOf(this)
    this.index := newVal.index
  }
}
