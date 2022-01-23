package scala.functional.programming.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {
  type Par[T] = ExecutorService => Future[T]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture[A]

  def async[A, B](f: A => B): A => Par[B] = a =>
    lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List()): Par[List[A]])((p, l) => map2(p, l)(_ :: _))
  }


  //TODO : make sure system nano time is what we are really looking for monitoring elapsed time
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      private val fa = a(es)
      private val fb = b(es)
      @volatile private var done = false

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = fa.cancel(mayInterruptIfRunning) || fb.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

      override def get(): C = get(Long.MaxValue, TimeUnit.DAYS)

      override def get(timeout: Long, unit: TimeUnit): C = {
        val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
        val started = System.nanoTime
        val a = fa.get(timeoutNanos, TimeUnit.NANOSECONDS)
        val elapsed = System.nanoTime - started
        val b = fb.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
        val parC = f(a, b)
        done = true
        parC
      }

      override def isDone: Boolean = done
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def run[A](a: Par[A])(s: ExecutorService): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}
