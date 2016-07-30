package com.us.dsb.explore.inference

import scala.reflect.{ClassTag => StandardClassTag}
import scala.annotation.unchecked.uncheckedVariance

import org.scalatest.FunSuite


/**
  * Demo of context-inferred type going through ClassTag parameter.
  */
class ContextInferredClasstagDemoTest extends FunSuite {

  class BaseThing
  case class SubthingOne() extends BaseThing
  case class SubthingTwo() extends BaseThing {
    def someMethod(): Unit = {}
  }

  type ModifiedClassTag[+T] = StandardClassTag[T @uncheckedVariance]

  def getSubthingViaTag[C <: BaseThing](label: String)(implicit classTag: ModifiedClassTag[C]): Option[C] = {
    println(s"$label: classTag = " + classTag)
    null.asInstanceOf[Some[C]]
  }

  test("Demo ...") {
    val v2: Option[SubthingOne] = getSubthingViaTag("2")


    def takesSpecificType(arg: Option[SubthingTwo]): Unit = {}
    val v4 = takesSpecificType(getSubthingViaTag("4"))

    // Infers C = Nothing.  (Error not caught until run-time.)
    //val v5: SubthingOne = getThingViaTag("5").get
  }

}
