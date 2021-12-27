package scala.functional.programming.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _ + 1)
  }

  def depth[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l, r) => (l max r) + 1)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(f.andThen(Leaf(_)): A => Tree[B])((l, r) => Branch(l, r))
  }

  def fold[A, B](tree: Tree[A])(map: A => B)(reduce: (B, B) => B): B = {
    tree match {
      case Branch(left, right) => reduce(fold(left)(map)(reduce), fold(right)(map)(reduce))
      case Leaf(value) => map(value)
    }
  }
}


