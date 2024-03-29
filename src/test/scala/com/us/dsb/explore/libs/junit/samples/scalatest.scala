/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.us.dsb.explore.libs.junit.samples

/*
ScalaTest facilitates different styles of testing by providing traits you can mix
together to get the behavior and syntax you prefer.  A few examples are
included here.  For more information, visit:

http://www.scalatest.org/

One way to use ScalaTest is to help make JUnit or TestNG tests more
clear and concise. Here's an example:
*/
import scala.collection.mutable.Stack
import org.scalatest._
//import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._
import org.junit.Test
import org.specs2.runner.JUnitRunner

//noinspection ReferenceMustBePrefixed
class StackSuite extends Assertions {

  @Test def stackShouldPopValuesIinLastInFirstOutOrder() = {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  @Test def stackShouldThrowNoSuchElementExceptionIfAnEmptyStackIsPopped() = {
    val emptyStack = new Stack[String]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}

/*
Here's an example of a FunSuite with ShouldMatchers mixed in:
*/
import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest._
//import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class ListSuite extends AnyFunSuite /*with Matchers/*ShouldMatchers*/*/ {

  test("An empty list should be empty") {
    List() should be (Symbol("empty"))
    Nil should be (Symbol("empty"))
  }

  test("A non-empty list should not be empty") {
    List(1, 2, 3) should not be Symbol("empty")
    List("fee", "fie", "foe", "fum") should not be Symbol("empty")
  }

  test("A list's length should equal the number of elements it contains") {
    List() should have length 0
    List(1, 2) should have length 2
    List("fee", "fie", "foe", "fum") should have length 4
  }
}

/*
ScalaTest also supports the behavior-driven development style, in which you
combine tests with text that specifies the behavior being tested. Here's
an example whose text output when run looks like:

A Map
- should only contain keys and values that were added to it
- should report its size as the number of key/value pairs it contains
*/
import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable.Stack

class ExampleSpec extends AnyFunSpec {

  describe("A Stack") {

    it("should pop values in last-in-first-out order") {
      val stack = new Stack[Int]
      stack.push(1)
      stack.push(2)
      assert(stack.pop() === 2)
      assert(stack.pop() === 1)
    }

    it("should throw NoSuchElementException if an empty stack is popped") {
      val emptyStack = new Stack[Int]
      intercept[NoSuchElementException] {
        emptyStack.pop()
      }
    }
  }
}
