package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val wholeNumber = (x: Int) => x >= 0
    val countingNumber = (x: Int) => x > 0
    val integers = (x: Int) => true
    val evenNumber = (x: Int) => x % 2 == 0

  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Not in singletonSet(1)")

      assert(contains(wholeNumber, 0), "Zero is in whole numbers")
      assert(contains(wholeNumber, 100), "100 is in whole numbers")
      assert(!contains(wholeNumber, -1), "-1 is not in whole numbers")

      assert(!contains(countingNumber, 0), "Zero is not in counting numbers")
      assert(contains(countingNumber, 100), "100 is in counting numbers")
      assert(!contains(countingNumber, -1), "-1 is not in counting numbers")

      assert(contains(integers, 0), "Zero is in integers")
      assert(contains(integers, 100), "100 is in integers")
      assert(contains(integers, -1), "-1 is not in integers")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val unionOfWholeAndCountingNumber = union(wholeNumber, countingNumber)
      assert(contains(unionOfWholeAndCountingNumber, 0), "Zero is in union of whole and counting number")
      assert(contains(unionOfWholeAndCountingNumber, 100), "100 is in union of whole and counting number")
      assert(!contains(unionOfWholeAndCountingNumber, -1), "-1 is not in union of whole and counting number")
    }
  }

  test("intersect contains common elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")

      val intersectionOfWholeAndCountingNumber = intersect(wholeNumber, countingNumber)
      assert(!contains(intersectionOfWholeAndCountingNumber, 0), "Zero is not in intersection of whole and counting number")
      assert(contains(intersectionOfWholeAndCountingNumber, 100), "100 is in intersection of whole and counting number")
      assert(!contains(intersectionOfWholeAndCountingNumber, -1), "-1 is not in intersection of whole and counting number")
    }
  }

  test("diff contains elements in A that are not in B") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")

      val diffOfWholeAndCountingNumber = diff(wholeNumber, countingNumber)
      assert(contains(diffOfWholeAndCountingNumber, 0), "Zero is in diff of whole and counting number")
      assert(!contains(diffOfWholeAndCountingNumber, 100), "100 is not in diff of whole and counting number")
      assert(!contains(diffOfWholeAndCountingNumber, -1), "-1 is not in diff of whole and counting number")
    }
  }

  test("filter odd numbers") {
    new TestSets {
      val odds = filter(integers, (x: Int) => x % 2 != 0)
      assert(contains(odds, 1), "1 is an odd number")
      assert(contains(odds, 10671), "10671 is an odd number")
      assert(!contains(odds, 248682), "248682 is not an odd number")
      assert(contains(odds, -10671), "-10671 is an odd number")
      assert(!contains(odds, -248682), "-248682 is not an odd number")
      assert(!contains(odds, 0), "0 is not an odd number")
    }
  }

  test("validate forall") {
    new TestSets {
      assert(forall(evenNumber, (x: Int) => x % 2 == 0), "forall even")
      assert(!forall(union(evenNumber, s1), { x: Int => x % 2 == 0 }), "forall union of even and 1")
      assert(forall(union(evenNumber, s2), { x: Int => x % 2 == 0 }), "forall union of even and 2")
    }
  }

  test("validate exist") {
    new TestSets {
      assert(exists(evenNumber, (x: Int) => x % 2 == 0), "even exists in evenNumber")
      assert(exists(union(evenNumber, s1), { x: Int => x % 2 == 0 }), "even exists in union of even and 1")
      assert(exists(union(evenNumber, s1), { x: Int => x == 1 }), "1 exists in union of even and 1")
      assert(!exists(union(evenNumber, s1), { x: Int => x == 3 }), "3 does not exists in union of even and 1")
    }
  }

  test("validate map") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      val doubledS123 = map(s123, (x: Int) => 2 * x)
      assert(!contains(doubledS123, 1), "1 is not in doubled element set");
      assert(!contains(doubledS123, 3), "3 is not in doubled element set");
      assert(contains(doubledS123, 2), "2 is in doubled element set");
      assert(contains(doubledS123, 4), "4 is in doubled element set");
      assert(contains(doubledS123, 6), "6 is in doubled element set");
    }
  }
}
