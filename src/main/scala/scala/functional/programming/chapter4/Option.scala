package scala.functional.programming.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatmap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false))
      this
    else
      None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatmap (oa => b flatmap (ob => Some(f(oa, ob))))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.contains(None))
      None
    else
      ???
  }
}