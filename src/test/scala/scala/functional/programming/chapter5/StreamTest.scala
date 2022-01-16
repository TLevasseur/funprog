package scala.functional.programming.chapter5

import org.junit.Assert._
import org.junit.Test

class StreamTest {
  @Test
  def testTake(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertEquals(Stream.apply(1, 2, 3).toList, s.take(3).toList)
    assertEquals(Empty.toList, s.take(0).toList)
  }

  @Test
  def testDrop(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertEquals(Stream.apply(4, 5).toList, s.drop(3).toList)
    assertEquals(Empty.toList, s.drop(5).toList)
    assertEquals(s.toList, s.drop(0).toList)
  }

  @Test
  def testTakeWhile(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertEquals(Stream.apply(1, 2, 3).toList, s.takeWhile(e => e < 4).toList)
    assertEquals(s.toList, s.takeWhile(_ => true).toList)
    assertEquals(Empty.toList, s.takeWhile(_ => false).toList)
  }

  @Test
  def testForAll(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertTrue(s.forAll(_ > 0))
    assertFalse(s.forAll(_ < 5))
  }

  @Test
  def headTest(): Unit = {
    assertEquals(1, Stream.apply(1, 2, 3, 4, 5).headOption.get)
    assertFalse(Stream.apply().headOption.isDefined)
  }

  @Test
  def testFilter(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertEquals(Stream.apply(1, 3, 5).toList, s.filter(_ % 2 == 1).toList)
  }

  @Test
  def testMap(): Unit = {
    val s = Stream.apply(1, 2, 3, 4, 5)
    assertEquals(Stream.apply(1, 0, 1, 0, 1).toList, s.map(_ % 2).toList)
  }

  @Test
  def testAppend(): Unit = {
    val s = Stream.apply(1, 2, 3, 4)
    assertEquals(Stream.apply(1, 2, 3, 4, 5).toList, s.append(Stream.apply(5)).toList)
  }

  @Test
  def testFlatMap(): Unit = {
    val s = Stream.apply(1, 2, 3, 4)
    assertEquals(List(0, 1, 0, 2, 0, 3, 0, 4), s.flatMap(Stream.apply(0, _)).toList)
  }

  @Test
  def testConstant(): Unit = {
    assertEquals(List(1, 1, 1, 1, 1), Stream.constant(1).take(5).toList)
  }

  @Test
  def testFrom(): Unit = {
    assertEquals(List(6, 7, 8, 9, 10), Stream.from(6).take(5).toList)
  }

  @Test
  def testFib(): Unit = {
    assertEquals(List(0, 1, 1, 2, 3, 5, 8), Stream.fibs().take(7).toList)
  }

}
