package parser

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  /*************************************************
    * Primitives:
    ************************************************/
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Always try p2 if p1 fails
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // Commit to p1 if the first A of p1 passes
  def attempt[A](p1: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def fail[A](msg: String): Parser[A]

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  /****************************************************/

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List[A]())

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    for {
      pa <- p
      pb <- p2
    } yield (pa, pb)
  }

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    for {
      pa <- p1
      pb <- p2
    } yield f(pa, pb)
  }

  def map[A, B](p1: Parser[A])(f: A => B): Parser[B] = {
    p1.flatMap((a) => succeed(f(a)))
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a, l) => a :: l)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(List[A]())
  }

  def letter: Parser[String] = "[a-zA-Z]".r
  def digits: Parser[String] = "\\d+".r
  def whitespace: Parser[String] = "\\s*".r
  def whitespace1: Parser[String] = "\\s+".r

  /**
    * Skips the second parameter
    */
  def skipRight[A](p: Parser[A], pSkip: => Parser[Any]): Parser[A] = {
    map2(p, slice(pSkip))((a, _) => a)
  }

  /**
    * Skips the first parameter
    */
  def skipLeft[A](pSkip: Parser[Any], p: => Parser[A]): Parser[A] = {
    map2(slice(pSkip), p)((_, a) => a)
  }

  def separator[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = {
    separator1(p, sep) | succeed(List[A]())
  }

  /**
    * Atleast one p separated by sep
    */
  def separator1[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = {
    map2(p, many(attempt(sep *> p)))((a, b) => a :: b)
  }

  def surround[A](start: Parser[Any], end: Parser[Any])(p: => Parser[A]): Parser[A] = {
    start *> p <* end
  }

  def parseListOfLen[A](p: Parser[A]): Parser[List[A]] = {
    for {
      len <- digits
      res <- listOfN(len.toInt, p)
    } yield res
  }

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def oneOf(s: List[Char]): Parser[Char] = {
    if (s.isEmpty)
      fail("oneOf empty List")

    else
      s.tail.foldLeft(char(s.head))((p, c) => p.or(char(c))) label s"oneOf $s"
  }

  def oneOf(s: String): Parser[Char] = oneOf(s.toCharArray.toList)

  def noneOf(s: String): Parser[Char] = {
    if (s.length == 0)
      fail("noneOf empty string")
    else
      s"[^$s]".r.map(_.charAt(0))
  }

  def quotedString: Parser[String] =
    token("\"" *> ".*?\"".r.map(_.dropRight(1)) label "quotedString")

  def escapedQuotedString: Parser[String] = {
    val escapedChars = (char('\\') *> oneOf(List('\\', '"', 'n', 'r', 't')))
      .map {
        case '\\' => '\\'
        case '"' => '"'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
      }

    val stringPart = many(escapedChars | noneOf("\\\"")).map(_.mkString(""))
    ("\"" *> stringPart <* "\"") label "escaped quoted string"
  }

  def integerString: Parser[String] =
    "[-+]?([0-9]+)".r

  def integer: Parser[Int] =
    integerString.map(_.toInt)

  def doubleString: Parser[String] =
    "[-+]?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?".r

  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  /** p followed by whitespace */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipLeft(p, p2)
    def <*[B](p2: => Parser[B]): Parser[A] = self.skipRight(p, p2)

    def as[B](b: B): Parser[B] = p.map(_ => b)

    def separator[B](p2: Parser[B]) = self.separator(p, p2)
    def separator1[B](p2: Parser[B]) = self.separator1(p, p2)

    def label(msg: String) = self.label(msg)(p)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n') match {
    case -1 => offset + 1
    case c => c
  }
  val res = currentLine + "\n" +
    (" " * (col - 1)) + "^\n\n"

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def inputWithOffset(): String = input.drop(offset)
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(location: Location, msg: String): ParseError = copy(stack = (location, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  override def toString: String = {
    val context = stack.lastOption.map { case (location, _) =>
      location.currentLine + '\n' +
        (" " * (location.col - 1)) + "^\n\n"
    }
    val errors = stack.map { case (l, msg) =>
      s"[${l.line}:${l.col}] -> $msg"
    }.mkString("\n")
    context.getOrElse("") + errors
  }

}
