package scala.functional.programming.chapter8

import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}
class GenTest {

  @Test
  def testAND(): Unit = {
    Assert.assertTrue((Prop(true) && Prop(true)).check)
    Assert.assertFalse((Prop(false) && Prop(true)).check)
    Assert.assertFalse((Prop(true) && Prop(false)).check)
    Assert.assertFalse((Prop(false) && Prop(false)).check)
  }
}
