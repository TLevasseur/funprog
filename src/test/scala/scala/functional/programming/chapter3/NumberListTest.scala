package scala.functional.programming.chapter3

import org.junit.Assert._
import org.junit._

import scala.functional.programming.chapter3.NumberList._

@Test
class NumberListTest {

  @Test
  def zipSumTest(): Unit = {
    assertEquals(zipSum(List(1, 2, 3), List(4, 5, 6)), List(5, 7, 9))
  }

  @Test
  def testRemoveOddsViaFlatMap(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(2, 4)
    assertEquals(removeOddsViaFlatMap(list), list2)
  }

  @Test
  def testRemoveOdds(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(2, 4)
    assertEquals(removeOdds(list), list2)
  }


  @Test
  def testConcatenate(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(2, 3, 4, 5)
    assertEquals(addOne(list), list2)
  }

  @Test
  def testListToString(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List("1", "2", "3", "4")
    assertEquals(listToString(list), list2)
  }


}


