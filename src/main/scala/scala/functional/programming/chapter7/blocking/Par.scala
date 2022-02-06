package scala.functional.programming.chapter7.blocking

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[T] = ExecutorService => Future[T]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture[A](a)

  def async[A, B](f: A => B): A => Par[B] = a =>
    lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List()): Par[List[A]])((p, l) => map2(p, l)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(async(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val ans = ps.map(async(a => {
      if (f(a))
        List(a)
      else
        List()
    }))
    map(sequence(ans))(_.flatten)
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = {
    map2(a, unit())((a, _) => f(a))
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(toTuple(a, b), c)((ab, c1) => f(ab._1, ab._2, c1))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(toTuple(a, b), toTuple(c, d))((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))
  }

  private def toTuple[A, B](a: Par[A], b: Par[B]): Par[(A, B)] = {
    map2(a, b)((_, _))
  }

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

  def reduce[A](list: List[A])(f: (A, A) => A): Par[A] = {
    if (list.size == 1)
      lazyUnit(list.head)
    else {
      val (left, right) = list.splitAt(list.size / 2)
      map2(reduce(left)(f), reduce(right)(f))(f)
    }
  }

  def fold[A, B](list: List[A])(initValue: B)(f: (A, B) => B): Par[B] = {
    if (list.isEmpty)
      lazyUnit(initValue)
    else
      map2(lazyUnit(list.head), fold(list.tail)(initValue)(f))(f)
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
