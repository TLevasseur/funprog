package scala.functional.programming.chapter5

import scala.annotation.tailrec
import scala.functional.programming.chapter5.Stream.cons

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(None: Option[A])((e, _) => Some(e))

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

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) =>
    if (p(a))
      Cons(() => a, () => b)
    else
      Empty
  )

  final def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => b && p(a))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => {
      if (f(a))
        cons(a, b)
      else
        b
    })

  def append[B >: A](e: => Stream[B]): Stream[B] = {
    foldRight(e)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
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

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibs(): Stream[Int] = unfold((0, 1))(tuple => Some(tuple._1, (tuple._2, tuple._1 + tuple._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, b)) => cons(a, unfold(b)(f))
    }
  }

}