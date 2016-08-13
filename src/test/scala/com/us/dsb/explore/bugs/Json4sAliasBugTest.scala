package com.us.dsb.explore.bugs

import org.json4s.DefaultFormats
import org.json4s.Extraction.decompose
import org.scalatest.FunSuite

/**
 * Test case for Json4s "Bug: Extraction's reflection breaks on type aliases
 * (loops endlessly or throws)" (https://github.com/json4s/json4s/issues/397).
 */
class Json4sAliasBugTest extends FunSuite {

  implicit val fmts = DefaultFormats

  object SomeObject {
    type Alias1 = Long
  }
  type Alias2 = Long

  class C1(val f: List[SomeObject.Alias1])
  class C2(val f: List[Alias2])

  test("Bug case: expect endless loop or 'MappingException: Unexpected type" +
       " info TypeRefType...' (for <3.4.0 or 3.4.0, respectively).") {
    val j = decompose(new C1(List()))
  }

  test("A working case.") {
    val j = decompose(new C2(List()))
  }
}
