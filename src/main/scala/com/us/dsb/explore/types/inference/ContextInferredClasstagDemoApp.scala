package com.us.dsb.explore.types.inference

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{ClassTag => OriginalClassTag}

object ContextInferredClasstagDemoApp {

  class Thing
  case class SubthingOne() extends Thing
  case class SubthingTwo() extends Thing {
    def someMethodxx(): Unit = {}
  }


  type ClassTagMod[+T] = OriginalClassTag[T @uncheckedVariance]

  import System.err.println


  def getThingViaTagxx[Sub <: Thing](label: String)(implicit classTagxx: ClassTagMod[Sub]): Sub = {
    val requestedClass = classTagxx.runtimeClass
    println(s"$label: requestedClass: ${requestedClass.getSimpleName}")
    null.asInstanceOf[Sub]
  }

  def main(args : Array[String]): Unit = {

    // Explicit-type case (works even with original ClassTag)
    getThingViaTagxx[SubthingOne]("1")

    // Context-inferred--type case (works only with modified ClassTag)
    val v2: Option[SubthingTwo] = getThingViaTagxx("2")


    def takesSpecificType(arg: Option[SubthingTwo]): Unit = {}
    val v4 = takesSpecificType(getThingViaTagxx("4"))

    // Infers C = Nothing.  (Error not caught until run-time.)
    //val v5: SubthingOne = getThingViaTag("5:").get

  }

}
