package scala.functional.programming.chapter9

import scala.language.higherKinds

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]

  def succeed[A](a: A): Parser[A] = ParserOps(string("")).map(_ => a)

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = this or p2

    def or[B >: A](p2: Parser[B]): Parser[B] = this or p2

    def map[B](f: A => B): Parser[B] = ???

    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = ???

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
      ParserOps(product(p2)).map(f.tupled)
    }

    def many: Parser[List[A]] = ParserOps(many1) or succeed(List())

    def many1: Parser[List[A]] = map2(many)(_ :: _)

    def listOfN(n: Int): Parser[List[A]] =
      if (n > 0)
        map2(listOfN(n - 1))(_ :: _)
      else
        succeed(List())

  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next()
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}