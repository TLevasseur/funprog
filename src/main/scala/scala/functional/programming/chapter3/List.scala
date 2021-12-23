package scala.functional.programming.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tailingList: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def concatenate[A](as: List[A], az: List[A]): List[A] = {
    foldLeft(az, as)((acc, e) => append(acc, e))
  }

  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, Cons(a, Nil))((a, l) => Cons(a, l))
  }

  def sum(as: List[Int]): Int = {
    foldLeft(as, 0)((a, b) => a + b)
  }

  def product(as: List[Int]): Int = {
    foldLeft(as, 1)((a, b) => a * b)
  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((b, _) => b + 1)
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, Nil) => f(z, x)
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(value, x) => Cons(value, init(x))
    }
  }

  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, t) => {
        if (f(x)) {
          dropWhile(t, f)
        } else {
          Cons(x, t)
        }
      }
    }
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n == 0) {
      list
    } else {
      drop(tail(list), n - 1)
    }
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(_, x) => x
    }
  }


  def setHead[A](list: List[A], value: A): List[A] = {
    list match {
      case Nil => Cons(value, Nil)
      case Cons(_, x) => Cons(value, x)
    }
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((element, list) => Cons(f(element), list))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((e, list) => {
      if (f(e)) {
        list
      } else
        Cons(e, list)
    })
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((element, list) => concatenate(f(element), list))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(e =>
      if (f(e))
        List()
      else
        List(e)
    )
  }


  def zipWith[A, B, C](list: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    (list, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }

  @scala.annotation.tailrec
  def hasSubSequence[A](list: List[A], l: List[A]): Boolean = {
    def customZipWith(list: List[A], list2: List[A]): List[Boolean] = {
      (list, list2) match {
        case (_, Nil) => List(true)
        case (Nil, _) => List(false)
        case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 == x2, customZipWith(xs1, xs2))
      }
    }

    if (foldLeft(customZipWith(list, l), true)(_ && _))
      true
    else
      list match {
        case Nil => false
        case Cons(_, xs) => hasSubSequence(xs, l)
      }
  }
}
