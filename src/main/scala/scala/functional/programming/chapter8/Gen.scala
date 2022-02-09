package scala.functional.programming.chapter8

import scala.functional.programming.chapter6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatmap[B](f: A => Gen[B]): Gen[B] = f(this.sample)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatmap (e => Gen.listOfN(e, this))
}


object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop((t: TestCases, rng: RNG) => {
      this.run(t, rng) match {
        case Passed => p.run(t, rng)
        case Falsified(failure, successes) => Falsified(failure, successes)
      }
    })

    def ||(p: Prop): Prop = Prop((t: TestCases, rng: RNG) => {
      this.run(t, rng) match {
        case Falsified(_, _) => p.run(t, rng)
        case Passed => Passed
      }
    })
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
