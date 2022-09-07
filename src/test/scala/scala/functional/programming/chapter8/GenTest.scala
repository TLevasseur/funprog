package scala.functional.programming.chapter8

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.{Assert, Test}

import scala.functional.programming.chapter6.SimpleRNG
class GenTest {

  @Test
  def testAND(): Unit = {
    Assert.assertTrue((Prop(true) && Prop(true)).check)
    Assert.assertFalse((Prop(false) && Prop(true)).check)
    Assert.assertFalse((Prop(true) && Prop(false)).check)
    Assert.assertFalse((Prop(false) && Prop(false)).check)
  }

  @Test
  def testChoose() : Unit = {
    val (x, _) = Gen.choose(10,20).sample.run(SimpleRNG(12345678987654321L))
    assertTrue(x<20)
    assertTrue(x>=10)
  }
}
