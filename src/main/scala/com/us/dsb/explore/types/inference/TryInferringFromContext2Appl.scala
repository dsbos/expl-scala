package com.us.dsb.explore.types.inference

import scala.annotation.implicitNotFound

//import scala.reflect.ClassTag

object TryInferringFromContext2Appl {

  class BaseThing
  case class SubthingOne() extends BaseThing
  case class SubthingTwo() extends BaseThing {
    def someMethod(): Unit = {}
  }
  case class SpecificThingThree() extends BaseThing

  val things: List[BaseThing] = List(SubthingOne(), SubthingTwo())

  type ClassTag[+T] = scala.reflect.ClassTag[T @scala.annotation.unchecked.uncheckedVariance]

  import System.err.{println => println}


  @implicitNotFound("NotNothing: Sorry, type inference was unable to figure out the type. You need to provide it explicitly.")
  trait NotNothing[T]

  object NotNothing {
    private val evidence: NotNothing[Any] = new Object with NotNothing[Any]

    implicit def notNothingEv[T](implicit n: T =:= T): NotNothing[T] =
      evidence.asInstanceOf[NotNothing[T]]
  }
  import NotNothing._



  def getThingViaTag[C <: BaseThing](implicit classTag: ClassTag[C]/*????, notNothing: NotNothing[C]*/): Option[C] = {
    val requestedClass = classTag.runtimeClass
    println(s"requestedClass: ${requestedClass.getSimpleName}")
    val anyFirstMatch = things.filter(obj => requestedClass.isInstance(obj)).headOption
    val result = anyFirstMatch.map(o => o.asInstanceOf[C])
    println(result.fold("none")(_.toString))
    result
  }

  def main(args : Array[String]): Unit = {

    println("1:")
    getThingViaTag[SubthingOne]

    println("2:")
    val v2: Option[SubthingOne] = getThingViaTag

    println("3:")
    val v3: Option[SpecificThingThree] = getThingViaTag

    println("4:")
    def takesSpecificType(arg: Option[SubthingTwo]): Unit = {}
    val v4 = takesSpecificType(getThingViaTag)

    println("5:")
    // Infers C = Nothing.  (Error not caught until run-time.)
    //val v5: SubthingOne = getThingViaTag.get

  }

}
