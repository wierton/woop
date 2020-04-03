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

import scala.reflect._
import scala.reflect.runtime.{universe => ru}

class A extends Bundle {
  val f = UInt(32.W)
}
class B extends A {  }
class C extends Bundle {
  val e = 1
  val a = new A
  val b = new B
  def c = UInt(32.W)
  def d = UInt(32.W)
}

object TTT {
  def getType[T:ru.TypeTag](obj:T) = ru.typeOf[T]
  def getTypeTag[T:ru.TypeTag](obj:T) = ru.typeTag[T]
  // def getClass[T:Class](obj:T) = classOf[T]
  def getClassTag[T:ClassTag](obj:T) = classTag[T]

  def evalMemberValues[A](topLevelObj: A)(implicit c: ru.TypeTag[A]): Unit = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    def loop(obj: Any, tp: ru.Type): Unit = {
      println(s"INSPECTING: $tp:")
      val objMirror = mirror.reflect(obj)
      val members = tp.decls.filter(_.isPublic)
      members.foreach { m =>
        if (m.isTerm && m.isModule) {
          println(s"MODULE: $m")
          loop(mirror.reflectModule(m.asModule).instance, m.info)
        }
        else if (m.isTerm && !m.isConstructor && m.isMethod && m.typeSignature.paramLists.isEmpty && !m.typeSignature.takesTypeArgs) {
          val value = objMirror.reflectMethod(m.asMethod)()
          println(s"VAL/DEF: $m = $value")
        }
        else {
          println(s"OTHERS: $m")

        }
      }
    }
    loop(topLevelObj, c.tpe)
  }

  def getMember[T:ru.TypeTag:ClassTag](obj:T, name:String):Any = {
    val rm = ru.runtimeMirror(getClass.getClassLoader)
    val instanceMirror = rm.reflect(obj)
    val sym = ru.typeOf[T].declaration(ru.TermName(name)).asTerm
    // println(sym)
    val fieldMirror = instanceMirror.reflectField(sym)
    // println(fieldMirror.get)
    // fieldMirror.set(12)
    println(fieldMirror)
    fieldMirror.get
  }

  def test() = {
    println(getType(new C))
    println(getTypeTag(new C))
    println(getTypeTag(new C).tpe)

    // println(getClass(new C))
    println(getClassTag(new C))
    println(classOf[C])
    println((new C).getClass)
    println(scala.reflect.classTag[C])

    val c = new C
    evalMemberValues(c)
    /*
    println(a)
    println(getMember(a.asInstanceOf[A], "f"))
    getMember(a, "f")
    */

    // Iterable[ru.Symbol]
    println(getType(new C).declarations)
    for (decl <- getType(new C).declarations) {
      println(decl+","+decl.fullName+", "+decl.name+", "+decl.isPublic
        +","+decl.isMethod)
      // println(getMember(c, decl.fullName))
    }
    // ru.Symbol
    println(getType(new C).declarations.take(0))
  }
}

object Main {
  def main(args:Array[String]):Unit = {
    TTT.test()
    /*
    val top = args(0)
    val chiselArgs = args.slice(1, args.length)
    chisel3.Driver.execute(chiselArgs, () => {
      val clazz = Class.forName("woop.core."+top)
      val constructor = clazz.getConstructor()
      constructor.newInstance().asInstanceOf[Module]
    })
    */
  }
}
