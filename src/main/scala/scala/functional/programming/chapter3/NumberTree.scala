package scala.functional.programming.chapter3

object NumberTree {
  def max(tree: Tree[Int]): Int = {
    tree match {
      case Branch(left, right) => max(left) max max(right)
      case Leaf(value) => value
    }
  }
}
