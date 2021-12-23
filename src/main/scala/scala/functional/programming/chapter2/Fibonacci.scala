package scala.functional.programming.chapter2

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def go(n1: Int, n2: Int, n: Int): Int = {
      if (n == 0) {
        n1
      } else {
        go(n2, n1 + n2, n - 1)
      }
    }

    go(0, 1, n)
  }
}
