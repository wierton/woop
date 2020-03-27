package woop
package core

import chisel3._
import chisel3.util._
import woop.configs._
import woop.utils._
import woop.consts._

class SimDev extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new MemIO)
  })
}

class SOC_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
    val can_log_now = Input(Bool())
  })

  val core = Module(new Core)
  val imux = Module(new MemMux("imux"))
  val dmux = Module(new MemMux("dmux"))
  val dev = Module(new SimDev)
  val crossbar = Module(new CrossbarNx1(4))
  // val icache = Module(new SimICache)
  val icache = Module(new IMemCistern(conf.icache_stages))

  core.io.can_log_now := io.can_log_now
  imux.io.can_log_now := io.can_log_now
  dmux.io.can_log_now := io.can_log_now
  crossbar.io.can_log_now := io.can_log_now
  icache.io.can_log_now := io.can_log_now

  dev.io.clock := clock
  dev.io.reset := reset

  icache.io.br_flush := core.io.br_flush
  icache.io.ex_flush := core.io.ex_flush

  icache.io.in <> core.io.imem
  imux.io.in <> icache.io.out
  dmux.io.in <> core.io.dmem

  imux.io.cached   <> crossbar.io.in(0)
  imux.io.uncached <> crossbar.io.in(1)
  dmux.io.cached   <> crossbar.io.in(2)
  dmux.io.uncached <> crossbar.io.in(3)

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit

  if (conf.log_Top) {
    when (io.can_log_now) { dump() }
  }

  def dump():Unit = {
    printf("------------\n")
  }
}

class AXI4_EMU_TOP extends Module {
  val io = IO(new Bundle {
    val commit = new CommitIO
  })

  val core = Module(new Core)
  val imux = Module(new MemMux("imux"))
  val dmux = Module(new MemMux("dmux"))
  val dev = Module(new SimDev)
  val crossbar = Module(new CrossbarNx1(4))
  val icache = Module(new ICache)
  val i2sram = Module(new AXI42SRAM)
  val dcache = Module(new DCache)
  val d2sram = Module(new AXI42SRAM)

  dev.io.clock := clock
  dev.io.reset := reset

  imux.io.in <> core.io.imem
  dmux.io.in <> core.io.dmem
  imux.io.cached <> icache.io.in
  icache.io.out <> i2sram.io.in
  dmux.io.cached <> dcache.io.in
  dcache.io.out <> d2sram.io.in

  icache.io.flush := core.io.br_flush
  dcache.io.flush := core.io.br_flush

  i2sram.io.out    <> crossbar.io.in(0)
  imux.io.uncached <> crossbar.io.in(1)
  d2sram.io.out    <> crossbar.io.in(2)
  dmux.io.uncached <> crossbar.io.in(3)

  crossbar.io.out <> dev.io.in

  core.io.commit <> io.commit
}

class ZEDBOARD_TOP extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

class LOONGSON_TOP extends Module {
  val io = IO(new Bundle {
    val in = new MemIO
  })
}

import scala.reflect.runtime.{universe => ru}

class A extends Bundle {  }
class B extends A {  }
class C extends Bundle {
  val a = new A
  val b = new B
  def c = UInt(32.W)
  def d = UInt(32.W)
}

object Main {
  def getPublicFields(rootClass: Class[_]): Seq[java.lang.reflect.Method] = {
    // Suggest names to nodes using runtime reflection
    def getValNames(c: Class[_]): Set[String] = {
      if (c == rootClass) {
        Set()
      } else {
        getValNames(c.getSuperclass) ++ c.getDeclaredFields.map(_.getName)
      }
    }
    val valNames = getValNames(this.getClass)
    def isPublicVal(m: java.lang.reflect.Method) =
      m.getParameterTypes.isEmpty && valNames.contains(m.getName) && !m.getDeclaringClass.isAssignableFrom(rootClass)
    this.getClass.getMethods.sortWith(_.getName < _.getName).filter(isPublicVal(_))
  }
  def getBundleField(m: java.lang.reflect.Method): Option[Data] = m.invoke(this) match {
    case d: Data => Some(d)
    case Some(d: Data) => Some(d)
    case _ => None
  }

  def main(args:Array[String]):Unit = {
    for (m <- getPublicFields(classOf[C])) {
      getBundleField(m) match {
        case Some(d: Data) => {
          println(m.getName)
        }
        case None => {
          println("none." + m.getName)
        }
      }
    }

    val top = args(0)
    val chiselArgs = args.slice(1, args.length)
    chisel3.Driver.execute(chiselArgs, () => {
      val clazz = Class.forName("woop.core."+top)
      val constructor = clazz.getConstructor()
      constructor.newInstance().asInstanceOf[Module]
    })
  }
}
