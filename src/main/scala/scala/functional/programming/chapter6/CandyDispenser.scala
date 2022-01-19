package scala.functional.programming.chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def interact(input: Input): Machine = {
    if (this.candies == 0) {
      this
    } else
      input match {
        case Coin => if (this.locked) Machine(locked = false, candies, coins + 1) else this
        case Turn => if (this.locked) this else Machine(locked = true, candies - 1, coins)
      }
  }
}

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val apply = inputs.map(i => { m: Machine => m.interact(i) }).reduce(_ andThen _)
    val init = { m: Machine => ((m.candies, m.coins), m) }
    State[Machine, (Int, Int)](init compose apply)
  }
}

