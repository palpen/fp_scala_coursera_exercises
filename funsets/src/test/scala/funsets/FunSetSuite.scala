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
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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

   */

  trait TestSets {
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(1)
    val union1 = union(s1, s2)
    val union2 = union(s3, s4)
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
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test(testName = "Intersection contains only common elements in each set") {
    new TestSets {
      val s = intersect(s1, s4)
      assert(contains(s, 1), "Intersect 1")

      val s_int= intersect(union1, union2)
      assert(!contains(s_int, 2), "Intersect 2")
      assert(contains(s_int, 1), "Intersect 3")

    }
  }

  test(testName = "Difference contains only elements in one set and not the other") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")

      val s_diff2 = diff(union1, union2)
      assert(!contains(s_diff2, 1), "Diff 3")
      assert(contains(s_diff2, 2), "Diff 4")

    }
  }
  test(testName = "Filter by predicate") {
    new TestSets {
      val s = filter(s1, (x:Int) => (x>1))
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 0), "Filter 2")

      val s_union = union(s0, s1)
      val s_union_filtered = filter(s_union, (x:Int) => x<1)
      assert(contains(s_union_filtered, 0), "Filter 3")
      assert(!contains(s_union_filtered, 1), "Filter 4")
    }
  }

  test(testName = "For All") {
    def s1(x: Int) = x > 0
    def s2(x: Int): Boolean = 5 > x & x > -5
    def p1(x: Int) = 10 > x & x > -10

    assert(!forall(s1, p1), "For All 1")
    assert(forall(s2, p1), "For all 2")
  }

  test(testName = "Exists") {
    def s1(x: Int) = x > 0
    def s2(x: Int): Boolean = 5 > x & x > -5
    def s3(x: Int): Boolean = 55 > x & x > 50

    def p1(x: Int) = 10 > x & x > -10

    assert(exists(s1, p1))
    assert(exists(s2, p1))
    assert(!exists(s3, p1))
  }
}
