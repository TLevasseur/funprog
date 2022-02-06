package scala.functional.programming.chapter8

import scala.functional.programming.chapter6.{RNG, State}

object Gen {

  trait Prop {
    type SuccessCount = Int
    type FailedCase = String

    def check(): Either[(FailedCase, SuccessCount), SuccessCount]

    def &&(p: Prop): Prop = {
      ???
    }
  }


  case class Gen[A](sample: State[RNG, A])

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(rng => RNG.limit(rng, start, stopExclusive)))
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] = {
    ???
  }

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    ???
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = {
    ???
  }
}
