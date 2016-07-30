package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._


class TaggedValueTypesExplorationTest extends FunSuite {

  test("Tagged ...") {
    type Tagged[U] = { type Tag = U }
    type @@[T, U] = T with Tagged[U]

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


    ////////////////////////////////////////
    // Weak typing in unconstrained expression:
    val notA2 /* : Int */ = a1 + a1

    val b2 = 2.asInstanceOf[TypeBInt]
    a1 + b2

    ////////////////////
    // Weak typing in <what kind?> expression:
    if (a1 > b2) {}

    ////////////////////////////////////////
    // Broken (useless) typing in constrained expression:
    // ("a1 + a1" gets type Int (not TypeAInt), which can't be assigned to a2.)
    "val a2: TypeAInt = a1 + a1" shouldNot compile // (well, _does_ not)


    // For reducing ".asInstanceOf[...]" syntax:

    class Tagger[U] {
      def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
    }
    def tag[U] = new Tagger[U]

    val b5 = tag[TypeB](6)
    val b6: TypeBInt = b5
    val b7: Int @@ TypeB = b5
    //??? Why not?  Is like b6 = expression from b5.   (Says found Int(6), required: Int.):
    //val b8: TypeBInt = tag[TypeB](6)


    def tag1[Repr, T](a: Repr): Repr @@ T = a.asInstanceOf[Repr @@ T]

    //???? val b10: TypeBInt = tag1(4)

   // val b6: TypeBInt = tag1[TypeAInt](4)

  }




}
