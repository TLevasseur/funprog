package scala.functional.programming.chapter8

import scala.functional.programming.chapter6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatmap[B](f: A => Gen[B]): Gen[B] = f(this.sample)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatmap (e => Gen.listOfN(e, this))
}

trait Prop {
  type SuccessCount = Int
  type FailedCase = String

  def check(): Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = {
    ???
  }
}

object Gen {

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatmap(if (_) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double)).flatmap(rnd => if (rnd > (g1._2 / (g1._2 + g2._2))) g2._1 else g1._1)
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(rng => RNG.limit(rng, start, stopExclusive)))
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] = {
    ???
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = {
    ???
  }
}
