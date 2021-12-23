package scala.functional.programming.chapter3

import org.junit.Assert._
import org.junit._

import scala.functional.programming.chapter3.List._

@Test
class ListTest {


  @Test
  def testConcatenate(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(concatenate(list, list2), List(1, 2, 3, 4, 1, 2, 3))
  }

  @Test
  def testAppend(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(append(list2, 4), list)
    assertEquals(append(Nil, 1), Cons(1, Nil))
  }

  @Test
  def testSumAndProductWithFoldRight(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(foldRight(list, 0)((a, b) => a + b), 10)
    assertEquals(foldRight(list2, 0)((a, b) => a + b), 6)
    assertEquals(foldRight(list, 1)((a, b) => a * b), 24)
    assertEquals(foldRight(list2, 1)((a, b) => a * b), 6)
  }

  @Test
  def testSumAndProduct(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(sum(list), 10)
    assertEquals(sum(list2), 6)
    assertEquals(product(list), 24)
    assertEquals(product(list2), 6)
  }

  @Test
  def testLength(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(length(list), 4)
    assertEquals(length(list2), 3)
  }

  @Test
  def testInit(): Unit = {
    val list = List(1, 2, 3, 4)
    val list2 = List(1, 2, 3)
    assertEquals(init(list), list2)
  }

  @Test
  def testDropWhile(): Unit = {
    val list3 = List(1, 2, 3)
    val list2 = List(2, 3)
    assertEquals(dropWhile(list3, (_: Int) < 2), list2)
    assertEquals(dropWhile(list3, (_: Int) => false), list3)
    assertEquals(dropWhile(list3, (_: Int) => true), Nil)
  }

  @Test
  def testDrop(): Unit = {
    val list3 = List(1, 2, 3)
    val list2 = List(2, 3)
    assertEquals(drop(list3, 1), list2)
    assertEquals(drop(list3, 0), list3)
    assertEquals(drop(list3, 3), Nil)
    assertEquals(drop(Nil, 10), Nil)
  }

  @Test
  def testTail(): Unit = {
    val list3 = List(1, 2, 3)
    val list2 = List(2, 3)
    assertEquals(tail(list3), list2)
    assertEquals(tail(tail(tail(list3))), Nil)
    assertEquals(tail(Nil), Nil)
  }

  @Test
  def testSet(): Unit = {
    val list2 = List(2, 3)
    val list2_2 = List(3, 3)
    assertEquals(setHead(list2, 3), list2_2)
    assertEquals(setHead(Nil, 3), Cons(3, Nil))
  }

  @Test
  def flatMapTest(): Unit = {
    val list = List(1, 2, 3)
    val list2 = List(1, 1, 2, 2, 3, 3)
    assertEquals(flatMap(list)(i => List(i, i)), list2)
  }

  @Test
  def hasSubSequenceTest(): Unit = {
    assertTrue(hasSubSequence(List(1,2,3,4),List(1,2)))
    assertTrue(hasSubSequence(List(1,2,3,4),List(2,3)))
    assertTrue(hasSubSequence(List(1,2,3,4),List(4)))
    assertFalse(hasSubSequence(List(1,2,3,4),List(1,2,3,4,5)))
  }

}


