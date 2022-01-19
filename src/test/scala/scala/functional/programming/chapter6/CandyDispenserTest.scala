package scala.functional.programming.chapter6

import org.junit.Assert.assertEquals
import org.junit.Test

class CandyDispenserTest {

  @Test
  def testCandyDispenser(): Unit = {
    val m = Machine(locked = true, 5, 10)
    val ((candies, coins), machine) = CandyDispenser.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn, Turn)).run(Machine(locked = true, 5, 10))
    assertEquals(1, candies)
    assertEquals(14, coins)
  }
}
