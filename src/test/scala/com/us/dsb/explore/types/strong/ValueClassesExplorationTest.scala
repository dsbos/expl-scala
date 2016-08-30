//????PURGE THIS / MOVE THIS OUT

package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._


object ValueClassesExplorationTest {

  case class PoiId(value: Int) extends AnyVal

  case class SpNumber(value: Int) extends AnyVal

  case class MovingTime(value: Int) extends AnyVal
}


class ValueClassesExplorationTest extends FunSuite {
  // (Review:  I thought object's members were automatically visible in companion
  // class, but they weren't in this case.)
  import ValueClassesExplorationTest._


  case class Bundle(origPoi: PoiId, origSp: SpNumber)
  val somePoi = PoiId(111)
  val someSpNum = SpNumber(0)
  var anotherPoi: PoiId = PoiId(0)


  test("Strong typing for simple parameter passing and assignment.") {

    // Allows correct type:
    Bundle(somePoi, someSpNum)
    anotherPoi = somePoi

    // Catches wrong value type (of same underlying type):
    "Bundle(someSpNum, somePoi)" shouldNot typeCheck
    "anotherPoi = someSpNum" shouldNot typeCheck
    // Error:(...) type mismatch;
    //    found   : com.savi.spark.app.summarizer.TypeExplorationTest.SpNumber
    //    required: com.savi.spark.app.summarizer.TypeExplorationTest.PoiId

    // Catches wrong non-value type (underlying value type):
    "Bundle(42, someSp, 42)" shouldNot typeCheck
    "anotherPoi = 42" shouldNot typeCheck
    // Error:(...) type mismatch;
    //    found   : Int(42)
    //    required: com.us.dsb.explore.types.strong.ValueClassesExplorationTest.PoiId
  }


  val MOVING_TIME_THRESHOLDxx = MovingTime(45)
  val movingTime1 = MovingTime(30)
  val movingTime2 = MovingTime(60)
  val spNumber = SpNumber(30)


  test("NO strong typing for equality comparisions.") {

    // Allows correct tupe:
    assertResult(true)(movingTime1 == movingTime1)
    assertResult(false)(movingTime1 == movingTime2)

    // Fails to catch wrong type (worse, it always returns false).
    assertResult(false)(spNumber == movingTime1)
    assertResult(false)(spNumber == 30)              // !
    assertResult(false)(spNumber == spNumber.value)  // !
    // If warnings enabled:
    // "Warning:(...) comparing case class values of types ... SpNumber and
    //   MovingTime using `==' will always yield false
  }


  test("NO operations of underlying type available (sometimes good).") {

    "movingTime1 - movingTime1" shouldNot compile
    "movingTime1 * movingTime1" shouldNot compile
    "movingTime1 < movingTime1" shouldNot compile
    // "Error:(...) value < is not a member of ... MovingTime"

  }


}
