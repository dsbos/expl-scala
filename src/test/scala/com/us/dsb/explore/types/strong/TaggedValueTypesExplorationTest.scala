//????PURGE THIS / MOVE THIS OUT

package com.us.dsb.explore.types.strong

import org.scalatest.FunSuite
import org.scalatest.Matchers._


class TaggedValueTypesExplorationTest extends FunSuite {

  // (Review:  I thought object's members were automatically visible in companion
  // class, but they weren't in this case.)





  test("Tagged ...") {
    type Tagged[U] =  { type Tag = U }

    type @@[T, U] = T with Tagged[U]
    type NameThis[T, U] = T with Tagged[U]

    trait TypeA
    trait TypeB

    type TypeAInt = Int @@ TypeA
    type TypeBInt = Int @@ TypeB

    val a1: TypeAInt = 1.asInstanceOf[TypeAInt]
    val a2: TypeAInt = 2.asInstanceOf[TypeAInt]
    val a3 = a1 + a2
    if (a1 > a2) { /*...*/ }


    val b1: TypeBInt = 4.asInstanceOf[TypeBInt]
    "val b2: TypeBInt = a1" shouldNot typeCheck
    "val b3: TypeBInt = b1 + a1" shouldNot typeCheck
    val b4 = b1 + a1  // Hmm.  b2 is Int

    if (a1 > b1) { /*...*/ }  // Hmm.  (Compared as Int.)

    //

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
