package scala.functional.programming.chapter3

import org.junit.Assert._
import org.junit._

import scala.functional.programming.chapter3.NumberTree._
import scala.functional.programming.chapter3.Tree._

@Test
class TreeTest {
  @Test
  def testSize(): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))
    assertEquals(7, size(tree))
  }

  @Test
  def testMax(): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Leaf(1)))
    assertEquals(7, max(tree))
  }

  @Test
  def testDepth(): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Branch(Leaf(1), Leaf(2))))
    assertEquals(4, depth(tree))
  }

  @Test
  def testMap(): Unit = {
    val tree1 = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Leaf(1)))
    val tree2 = Branch(Branch(Leaf(10), Leaf(40)), Branch(Leaf(70), Leaf(10)))
    assertEquals(map(tree1)(_ * 10), tree2)
  }
}
