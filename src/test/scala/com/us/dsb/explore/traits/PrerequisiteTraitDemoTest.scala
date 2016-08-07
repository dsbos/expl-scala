package com.us.dsb.explore.traits

import org.scalatest.FunSuite
import org.scalatest.Matchers._


/**
  * Test to probe text on p. 234 of _Programming in Scala 3rd. ed._, that says
  * "... trait can only by mixed into a class that also extends [traits mixed
  * into that trait] ..."
  * */
class PrerequisiteTraitDemoTest extends FunSuite {

  // Trait required by some other trait.
  trait RequiredTrait

  // Trait requiring first trait, attempted with required trait in "extends"
  // clause (per text in question).
  trait RequiringTrait1 extends RequiredTrait {
  }

  // Trait requiring first trait, done using self type.
  trait RequiringTrait2 {
    self: RequiredTrait =>
  }


  // 0. Confirm probing method:
  class DoesntHaveRequiredTraitYet
  "(null: DoesntHaveRequiredTraitYet): RequiredTrait" shouldNot typeCheck
  // Error has been:
  // "type mismatch; found   : ...DoesntHaveRequiredTraitYet
  //                 required: ...RequiredTrait"

  class HasRequiredTraitAlready extends RequiredTrait  // (compiles)
  (null: HasRequiredTraitAlready): RequiredTrait       // (compiles)


  // 1. Show that mixing in RequiringTrait1 does NOT actualy require that a
  test("Demo a requiring trait that also adds the required trait.") {

    // class mixing it in already/otherwise has RequiredTrait (contrary to what
    // the book's text says) but actually adds RequiredTrait too (as expected):

    class AddsRequiredTrait extends RequiringTrait1
    (null: AddsRequiredTrait): RequiredTrait
  }

  // 2. Show that mixing in RequiringTrait2 DOES require that a class mixing
  // it in already/otherwise has RequiredTrait:
  test("Demo a requiring trait that really just requires the required trait.") {

    "class DoesNotHaveOrAddAddRequiredTrait extends RequiringTrait2" shouldNot compile
    // Error has been:
    // "illegal inheritance; self-type DoesNotHaveOrAddAddRequiredTrait does not
    // conform to ...RequiringTrait2's selftype ...RequiringTrait2 with
    // ...RequiredTrait"

    class HasRequiredTraitOtherwise extends RequiredTrait with RequiringTrait2
    (null: HasRequiredTraitOtherwise): RequiredTrait


    // TODO:  To be explored further:  Somewhat unexpectedly, the required trait
    // can appear later in the CompoundType (after "extends") than the trait in
    // question does.  (How does that affect linearization and cross-trait
    // constructor and method call order?)

    class ListsRequiredAfterRequiring extends RequiringTrait2 with RequiredTrait
    (null: ListsRequiredAfterRequiring): RequiredTrait
  }

}
