package woop

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.core._

import scala.reflect._
import scala.reflect.runtime.{universe => ru}

object printv {
  val defaultFmtMap = Map[String,String](
    "func" -> "%d", "et" -> "%d", "code" -> "%d",
    "instr" -> "%x"
    )

  def getTypeFromString(name: String): ru.Type = {
    val c = Class.forName(name)
    val mirror = ru.runtimeMirror(c.getClassLoader)
    val sym = mirror.staticClass(name)
    sym.selfType
  }

  def traverseMemberValues[T](topLevelObj: T, fmtmap:Map[String,String])(implicit c: ru.TypeTag[T]) = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    def traverse(obj: Any, tp: ru.Type): (String, Seq[Bits]) = {
      if (tp =:= ru.typeOf[UInt] || tp =:= ru.typeOf[SInt]) {
        return ("%x", Seq[Bits](obj.asInstanceOf[Bits]))
      }

      val objMirror = mirror.reflect(obj)
      var members = tp.members.filter(m => m.isPublic && m.isMethod && m.asMethod.returnType <:< ru.typeOf[Data])
        .filter(m => !m.isConstructor && m.isMethod && m.info.paramLists.isEmpty && !m.info.takesTypeArgs)
      var fmtString = ""
      var fmtBits = Seq[Bits]()
      var isBits = false
      if (tp <:< ru.typeOf[ValidIO[_]]) {
        val valid = objMirror.reflectMethod(members.filter(
          _.name.toString == "valid").head.asMethod)()
        fmtString = fmtString+"[%b]"
        fmtBits=fmtBits++Seq[Bits](valid.asInstanceOf[Bits])
        members = members.filter(_.name.toString == "bits")
        isBits = true
      } else if (tp <:< ru.typeOf[DecoupledIO[_]]) {
        val valid = objMirror.reflectMethod(members.filter(
          _.name.toString == "valid").head.asMethod)()
        val ready = objMirror.reflectMethod(members.filter(
          _.name.toString == "ready").head.asMethod)()
        fmtString = fmtString+"[%b,%b]"
        fmtBits=fmtBits++Seq[Bits](valid.asInstanceOf[Bits], ready.asInstanceOf[Bits])
        members = members.filter(_.name.toString == "bits")
        isBits = true
      }
      members.foreach { m => 
        if (!(m.asMethod.returnType =:= tp) && m.name.toString != "cloneType" && m.name.toString != "io") {
          val value = objMirror.reflectMethod(m.asMethod)()
          val info = if(isBits)
            getTypeFromString(value.getClass.getName)
            else m.asMethod.returnType
          val name = if(isBits) "" else m.name.toString
          if (fmtmap.contains(name)) {
            fmtString = fmtString+name+"="+
              fmtmap.getOrElse(name, "%x") + " "
            fmtBits = fmtBits++Seq[Bits](value.asInstanceOf[Data].asUInt)
          } else if ((info <:< ru.typeOf[ValidIO[_]]) ||
            (info <:< ru.typeOf[DecoupledIO[_]])) {
            val ret = traverse(value, info)
            fmtString = fmtString+name+ret._1.trim+" "
            fmtBits = fmtBits++ret._2
          } else if ((info <:< ru.typeOf[Bundle])) {
            val ret = traverse(value, info)
            fmtString = fmtString+name+"={"+
              ret._1.trim+"} "
            fmtBits = fmtBits++ret._2
          } else if (info <:< ru.typeOf[Bits]) {
            fmtString = fmtString+name+"=%x "
            fmtBits = fmtBits++Seq[Bits](value.asInstanceOf[Bits])
          }
        }
      }
      (fmtString.trim, fmtBits)
    }
    val ret = traverse(topLevelObj, c.tpe)
    ret
  }

  def apply[T:ru.TypeTag](sig:T, module:String, fmtmap:Map[String,String]=defaultFmtMap) = {
    val ret = traverseMemberValues(sig, fmtmap)
    val bits = Seq[Bits](GTimer())++ret._2
    printf("%d: "+module+": "+ret._1+"\n", bits:_*)
  }
}
