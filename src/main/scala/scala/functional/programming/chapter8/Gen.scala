package scala.functional.programming.chapter8


case class Prop(check: Boolean) {
  def &&(other: Prop): Prop = {
    new Prop(check && other.check)
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}
