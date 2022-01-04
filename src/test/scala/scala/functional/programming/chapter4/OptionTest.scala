package scala.functional.programming.chapter4

import org.junit.Assert._
import org.junit._


@Test
class OptionTest {

  @Test
  def testSequence(): Unit = {
    val list = List(Some(1), Some(2), Some(3))
    val list2 = Some(List(1, 2, 3))
    val list3 = List(Some(1), Some(2), None)
    assertEquals(list2, Option.sequence(list))
    assertEquals(None, Option.sequence(list3))
  }


}
