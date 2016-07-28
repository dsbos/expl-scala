package com.us.dsb.explore.types.inference

import scala.reflect.ClassTag

object TryInferringFromContextAppl {

  class GeneralThing
  case class SpecificThingOne() extends GeneralThing
  case class SpecificThingTwo() extends GeneralThing {
    def someMethod(): Unit = {}
  }

  val things: List[GeneralThing] = List(SpecificThingOne(), SpecificThingTwo())

  //import System.err.println
  def getThingViaTags[C <: GeneralThing](implicit classTag: ClassTag[C]): Option[C] = {
    println(s"classTag = $classTag")
    val requestedClass = classTag.runtimeClass
    val anyFirstMatch = things.filter(obj => requestedClass.isInstance(obj)).headOption
    anyFirstMatch.map(o => o.asInstanceOf[C])
  }


  def main(args : Array[String]): Unit = {

    // As expected/wanted, passed class tag is for SpecificThingOne:
    val v1 /*: Option[SpecificThingOne]*/ = getThingViaTags[SpecificThingOne]

    // Not as expected/wanted, passed class tag is for Nothing:
    val v2: Option[SpecificThingOne] = getThingViaTags
    // Why is getThingViaTags's C inferred to be type Nothing?
    // Does type inference ever propagate the declared type _from_ a type
    // ascription on a variable declaration "down" _to_ the initialization
    // expression?  If it does, why doesn't it do so here?

    // Not as expected/wanted, class class tag is for Nothing:
    def takesSpecificType(arg: Option[SpecificThingTwo]): Unit = {}
    val v3 = takesSpecificType(getThingViaTags)
  }

}
