package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scalaz.@@   // Note: Definition depends on ScalaZ version.


class ScalazTaggedTypesExplorationTest extends FunSuite {

  test("Tagged ...") {

    trait TypeA
    trait TypeB

    type TypeAInt = Int @@ TypeA
    type TypeBInt = Int @@ TypeB

    ////////////////////////////////////////
    // Strong typing at simple assignment:
    val a1: TypeAInt = 1.asInstanceOf[TypeAInt]
    val a1b: TypeAInt = a1
    "val b1: TypeBInt = a1" shouldNot compile
    "val a2: TypeAInt = 2" shouldNot compile

    ////////////////////
    // Seemingly good strong typing in unconstrained expression ...
    val b2 = 2.asInstanceOf[TypeBInt]
    "a1 - b2" shouldNot compile

    ////////////////////
    // ... but loses access to "base" type's methods (broken relative to what
    // I wanted--unmixable versions of types still having normal operations as
    // in VHDL and Ada integer types):
    "val a0: TypeAInt = a1 - a1" shouldNot compile // well, _doesn't, but should
    "a1 - a1" shouldNot compile // well, _doesn't, but should
    // yield "value - is not a member of TypeAInt"

    ////////////////////
    // Seemingly good strong typing in <what kind?> expression:
    "if (a1 > b2) {}" shouldNot compile

    ////////////////////
    // Don't forget some implicit AnyRef(?) -> String conversion:
    "a1 + a1" shouldNot compile  // (does not, but for interference reason)
  }

}
