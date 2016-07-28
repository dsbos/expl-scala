package com.us.dsb.explore.types.inference

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

  def getThingViaTag[C <: BaseThing](implicit classTag: ClassTag[C]): Option[C] = {
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
    //val v5: SubthingOne = getThingViaTag.get      // Infers C = Nothing

  }

}
