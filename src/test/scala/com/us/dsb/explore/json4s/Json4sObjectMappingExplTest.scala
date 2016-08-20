package com.us.dsb.explore.json4s

import org.json4s.jackson.Serialization
import org.json4s.{DefaultFormats, FieldSerializer}
import org.scalatest.FunSuite
import org.scalatest.Matchers._



class Json4sObjectMappingExplTest extends FunSuite {

  case class Class1A(val c1a_1: String, c1a_2: Class1B)
  case class Class1B(c1b_1: Int)

  test("Demo: Basic default ser./deser. of case class.") {
    val objTree1 = Class1A("one", Class1B(4))
    println("objTree1 = " + objTree1)

    implicit val formats = org.json4s.DefaultFormats
    val json1: String = Serialization.write(objTree1)
    println("json1 = " + json1)
    println()
  }


  case class Class2A(val c2a_1: String, c2a_2: Class2B) {
    var               c2a_3: Int = 123
    private       var c2a_4: Float = 1.3f
    private[this] var c2a_5: Long = 2L
  }
  case class Class2B(c2b_1: Int)

  val objTree2 = Class2A("one", Class2B(4))
  objTree2.c2a_3 = 3


  test("Demo: Default ser./deser. of case class ignores non-constructor fields.") {
    println("objTree23 = " + objTree2 + " (not showing non-constructor fields)")

    implicit val formats = org.json4s.DefaultFormats
    val json2a: String = Serialization.write(objTree2)
    println("json2a = " + json2a)
    println()
  }


  test("Demo: Adding FieldSerializer for class handles non-constructor fields.") {
    println("objTree23 = " + objTree2 + " (not showing non-constructor fields)")

    implicit val formats2 = DefaultFormats + FieldSerializer[Class2A]()

    val json2b: String = Serialization.write(objTree2)
    println("json2b = " + json2b)
    println()
  }


  /*

implicit val formats = org.json4s.DefaultFormats
println("xmp51JsonStr = " + xmp51JsonStr)

val xmp51Objs2: Something = Serialization.read[Something](xmp51JsonStr)
println("xmp51Objs2 = " + xmp51Objs2)


JsonMethods.parse(xmp51JsonStr)



test("Default maps only properties/attributes from constructor parameters") {

import org.json4s.jackson.Serialization




/*case*/ class Something(val f1: String, f2: SomethingElse) {
val f3: Int = 123
}
/*case*/ class SomethingElse(e3: Int)

val xmp51Objs = new Something("some string", new SomethingElse(321))
println("xmp51Objs = " + xmp51Objs)
/*

implicit val formats = org.json4s.DefaultFormats
val xmp51JsonStr: String = Serialization.write(xmp51Objs)
println("xmp51JsonStr = " + xmp51JsonStr)

val xmp51Objs2: Something = Serialization.read[Something](xmp51JsonStr)
println("xmp51Objs2 = " + xmp51Objs2)


JsonMethods.parse(xmp51JsonStr)

*/
// Renaming field/property (_inside_ given class, no:

import FieldSerializer._
implicit val formats2 = DefaultFormats /*+
FieldSerializer[Something](renameTo("f3", "fThree"), renameFrom("f3", "3f")) +
FieldSerializer[SomethingElse](renameTo("e3", "eThree"), renameFrom("e3", "3e"))*/
val xmp51JsonStr2: String = Serialization.write(xmp51Objs)
println("xmp51JsonStr2 = " + xmp51JsonStr2)

*/


}
