package scala.functional.programming.chapter2

import org.junit.Assert._
import org.junit._

import scala.functional.programming.chapter2.Fibonacci.fib

@Test
class FiboTest {

  @Test
  def testFib(): Unit = {
    assertEquals(fib(0), 0)
    assertEquals(fib(1), 1)
    assertEquals(fib(2), 1)
    assertEquals(fib(3), 2)
    assertEquals(fib(4), 3)
    assertEquals(fib(5), 5)
    assertEquals(fib(6), 8)
    assertEquals(fib(7), 13)
    assertEquals(fib(8), 21)
    assertEquals(fib(9), 34)
  }

}


