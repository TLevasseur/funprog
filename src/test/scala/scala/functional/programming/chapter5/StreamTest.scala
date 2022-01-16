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
}
