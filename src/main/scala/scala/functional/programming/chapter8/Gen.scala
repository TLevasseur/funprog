package scala.functional.programming.chapter8

import scala.annotation.tailrec
import scala.functional.programming.chapter6.{RNG, State}


case class Prop(check: Boolean) {
  def &&(other: Prop): Prop = {
    new Prop(check && other.check)
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.double).map(start + _ * (stopExclusive - start)).map(_.toInt))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @tailrec
    def helper(l: List[A], n: Int, g: Gen[A])(rng: RNG): (List[A], RNG) = {
      if (n == 0)
        (l, rng)
      else {
        val (x, newGen) = g.sample.run(rng)
        helper(l :+ x, n - 1, g)(newGen)
      }
    }

    Gen(State(helper(List(), n, g)))
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}
