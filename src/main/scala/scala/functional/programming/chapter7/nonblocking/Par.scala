package scala.functional.programming.chapter7.nonblocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Par {

  sealed trait Future[A] {
    private[nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) {
          cb(f(a))
        })
    }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] =
    _ => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override private[nonblocking] def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call: Unit = r
  })

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    override private[nonblocking] def apply(k: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(k(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(k(f(a, b)))
        }
      }
      p1(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceN[A](pa: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(pa)((i: Int) => choices(i))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => new Future[B] {
    def apply(k: B => Unit): Unit =
      pa(es) { a =>
        eval(es) {
          choices(a)(es)(k)
        }
      }
  }

  def join[A](a: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        a(es)(a2 => eval(es) {
          a2(es)(cb)
        })
    }
}
