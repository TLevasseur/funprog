package scala.functional.programming.chapter6

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class TestRNG {


  @Test
  def testNonNegativeInt(): Unit = {
    @scala.annotation.tailrec
    def test(rng: RNG, n: Int): Unit = {
      if (n > 0) {
        val (i, newrng) = rng.nonNegativeInt
        assertTrue(i + "is not > 0", i > 0)
        test(newrng, n - 1)
      }
    }

    val rng = SimpleRNG(12345678987654321L)
    test(rng, 100)
  }

  @Test
  def testNextDouble(): Unit = {
    @scala.annotation.tailrec
    def test(rng: RNG, n: Int): Unit = {
      if (n > 0) {
        val (d, newrng) = rng.nextDouble
        assertTrue(d + "is not > 0", d >= 0)
        assertTrue(d + "is not < 1", d < 1)
        test(newrng, n - 1)
      }
    }

    val rng = SimpleRNG(12345678987654321L)
    test(rng, 100)
  }

  @Test
  def testInt(): Unit = {
    val rng = SimpleRNG(12345678987654321L)
    val (list, rng1) = RNG.ints(6)(rng)
    assertEquals(6, list.length)
  }

}
