package scala.functional.programming.chapter8

import scala.functional.programming.chapter6.{RNG, State}
import scala.functional.programming.chapter8.Prop._


object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = {
    Prop((max: Int, n: Int, rng: RNG) =>
      run(max, n, rng) match {
        case Passed => other.run(max, n, rng)
        case _ => _
      })
  }

  def ||(other: Prop): Prop = {
    Prop((max: Int, n: Int, rng: RNG) =>
      run(max, n, rng) match {
        case Falsified(_,_) => other.run(max, n, rng)
        case _ => _
      })
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double)).flatMap(d => if (d < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.map(f).flatMap(_.sample))
  }
}

trait SGen[+A] {

}
