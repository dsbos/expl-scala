package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._


class ManualTaggedTypesExplorationTest extends FunSuite {

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

    class Taggerxx[U] {
      def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
    }
    def tagxx[U] = new Taggerxx[U]

    val b5xx = tagxx[TypeB](6)
    val b6xx: TypeBInt = b5xx
    val b7xx: Int @@ TypeB = b5xx
    //??? Why not?  Is like b6 = expression from b5.   (Says found Int(6), required: Int.):
    //val b8: TypeBInt = tag[TypeB](6)


    def tag1xx[Reprxx, Txx](bxx: Reprxx): Reprxx @@ Txx = bxx.asInstanceOf[Reprxx @@ Txx]

    //???? val b10: TypeBInt = tag1(4)

   // val b6: TypeBInt = tag1[TypeAInt](4)


    // ???? Look into scalaz verstino

  }




}
