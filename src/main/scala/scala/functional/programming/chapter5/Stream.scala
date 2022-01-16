package scala.functional.programming.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  final def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => if (n == 0)
        Empty
      else if (n == 1)
        Cons(h, () => Stream.empty)
      else
        Cons(h, () => t().take(n - 1))
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    if (n == 0)
      this
    else
      this match {
        case Empty => Empty
        case Cons(_, t) => t().drop(n - 1)
      }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h()))
        Cons(h, () => t().takeWhile(p))
      else
        Empty
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
