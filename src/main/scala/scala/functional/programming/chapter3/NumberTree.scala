package scala.functional.programming.chapter3

import scala.functional.programming.chapter3.Tree._

object NumberTree {
  def max(tree: Tree[Int]): Int = {
    fold(tree)(a => a)((a: Int, b: Int) => a max b)
  }
}
