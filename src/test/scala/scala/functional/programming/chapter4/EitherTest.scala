package scala.functional.programming.chapter4

import org.junit.Assert._
import org.junit._

class EitherTest {

  @Test
  def testSequence(): Unit = {
    val list = List(1, 2, 3, 4, 5)
    assertEquals(Right(List(1, 1, 1, 1, 1)), Either.traverse(list)(e => if (e != 0) Right(e / e) else Left("Do not divide by 0")))
    val list2 = List(1, 2, 3, 4, 5, 0)
    assertEquals(Left("Do not divide by 0"), Either.traverse(list2)(e => if (e != 0) Right(e / e) else Left("Do not divide by 0")))
  }
}
