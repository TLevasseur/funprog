package scala.functional.programming.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(None: Option[A])((e, _) => Some(e))

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  final def take(n: Int): Stream[A] = {
    Stream.unfold(this, n) {
      case (_, 0) => None
      case (Cons(h, t), i) => Some(h(), (t(), i - 1))
      case (_, _) => None
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
    Stream.unfold(this) {
      case Cons(h, t) => if (p(h()))
        Some(h(), t())
      else
        None
      case _ => None
    }
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
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => {
      if (f(a))
        Stream.cons(a, b)
      else
        b
    })

  def append[B >: A](e: => Stream[B]): Stream[B] = {
    foldRight(e)((a, b) => Stream.cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
  }

  def zipWith[B, C](that: Stream[B], f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, that)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(f(x1(), x2()), (xs1(), xs2()))
    }
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Empty, Cons(x2, xs2)) => Some(((None, Some(x2())), (Empty, xs2())))
      case (Cons(x1, xs1), Empty) => Some(((Some(x1()), None), (xs1(), Empty)))
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(((Some(x1()), Some(x2())), (xs1(), xs2())))
    }
  }

  def startWith[B >: A](that: Stream[B]): Boolean = {
    this.zipAll(that).takeWhile(_._2.isDefined).forAll { case (maybeA, maybeB) => maybeA == maybeB }
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(Some(this): Option[Stream[A]]) {
      case Some(Empty) => Some((Empty, None))
      case Some(Cons(h, t)) => Some((Cons(h, t), Some(t())))
      case _ => None
    }
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream.apply(z)))((a, b) => (f(a, b._1), Stream.apply(f(a, b._1)).append(b._2)))._2
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