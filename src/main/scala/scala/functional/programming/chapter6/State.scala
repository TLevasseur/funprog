package scala.functional.programming.chapter6

import scala.functional.programming.chapter6.State.unit

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.flatMap(b => unit(f(a, b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(s => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => {
    (a, s)
  })

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List[A]()))((a, b) => a.map2(b)(_ :: _))
  }

}