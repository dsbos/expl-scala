//????PURGE THIS / MOVE THIS OUT

package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._


object ValueClassesExplorationTest {

  case class PoiId(val value: Int) extends AnyVal

  case class SpNumber(val value: Int) extends AnyVal

  case class MovingTime(val value: Int) extends AnyVal

  case class StationaryTime(val value: Int) extends AnyVal

}


class ValueClassesExplorationTest extends FunSuite {
  //???? CHECK:  I thought companion object's members were visible in class.
  import ValueClassesExplorationTest._


  // (Review:  I thought object's members were automatically visible in companion
  // class, but they weren't in this case.)


  test("Demo that value classes provide strong typing for _simple_ parameter passing and assignment.") {

    case class Bundle(val origPoi: PoiId, val origSp: SpNumber)

    val somePoi = PoiId(111)
    val someSpNum = SpNumber(0)

    val rightOrder = Bundle(somePoi, someSpNum)

    // Trying to compile:
    //   val x2 = Bundle(someSpNum, somePoi)
    // yields:
    //   Error:(...) type mismatch;
    //    found   : com.savi.spark.app.summarizer.TypeExplorationTest.SpNumber
    //    required: com.savi.spark.app.summarizer.TypeExplorationTest.PoiId
    //       val x2 = Bundle(someSpNum, somePoi)
    //                       ^
    "val wrongOrder = Bundle(someSpNum, somePoi)" shouldNot typeCheck

    var curPoi: PoiId = PoiId(0)
    curPoi = somePoi
    // Trying to compile this yields error similar to above.
    "curPoi = someSpNum" shouldNot typeCheck
  }



  test("Demo that value classes don't seem good for ???.") { //???? NO; for expressions(?) (other than paramter passing and assignment)
    val MOVING_TIME_THRESHOLD = MovingTime(45)
    val movingTime = MovingTime(60)
    val movingTime2 = MovingTime(60)
    val stationaryTime = StationaryTime(30)

    // Unfortunately, equality comparison is not type checked.
    //
    // Worse, it compiles/ but always returns false.
    //

    assertResult(true)(movingTime == movingTime2)
    assertResult(false)(movingTime == MOVING_TIME_THRESHOLD)
    //assertResult(false)(stationaryTime == MOVING_TIME_THRESHOLD)  //?? No error; only warning.
    //assertResult(false)(stationaryTime == 123)  //?? No error; only warning.


    // Unfortunately, not type checked.  However, with warnings on, compiler
    // says:
    //   Warning:(...) comparing values of types com.savi.spark.app.summarizer.TypeExplorationTest.MovingTime and Int using `==' will always yield false
    //      println(movingTime == 60)
    //                             ^
    //?????println(movingTime == 60)
    //?????println(movingTime == 61)


    // Integer methods are not available at least not automically (or any other
    // simple way I've found):
    "if (movingTime >= MOVING_TIME_THRESHOLD) { /*...*/ }" shouldNot compile
    // That yields:
    //   Error:(...) value >= is not a member of com.savi.spark.app.summarizer.TypeExplorationTest.MovingTime




    // Unfortunately, not type checked.  However, with warnings on, compiler
    // says:
    //   Warning:(...) comparing values of types com.savi.spark.app.summarizer.TypeExplorationTest.MovingTime and Int using `==' will always yield false
    //      println(movingTime == 60)
    //                             ^
    //?????println(movingTime == 60)
    //?????println(movingTime == 61)

    //movingTime.value




    //  Shoot--number operations aren't visible (automatically
    //val movingTime2 = movingTime - movingTime
  }



}
