package parser
import ParserUtils._
import scala.util.matching.Regex

object ParserImplementation extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(parseError, _) => Left(parseError)
    }

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_,_) => f
    }

  def slice[A](p: Parser[A]): Parser[String] = l => p(l) match {
      case Success(_, i) => Success(l.inputWithOffset().take(i), i)
      case f: Failure => f
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.label(msg))

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e,false) => y(s)
      case r => r
    }

  def string(s: String): Parser[String] =
    l => {
      val nonMatchingIndex = firstNonMatching(l.input, l.offset, s)
      if (nonMatchingIndex == -1)
        Success(s, s.length)
      else
        Failure(l.advanceBy(nonMatchingIndex).toError("string: \"" + s + '"'), nonMatchingIndex != 0)
    }

  def regex(r: Regex): Parser[String] =
    l =>
      r.findPrefixOf(l.inputWithOffset()) match {
        case Some(string) =>
          Success(string, string.length)
        case _ =>
          Failure(l.toError(s"regex: $r"), isCommitted = false)
      }

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def fail[A](msg: String): Parser[A] =
    l => Failure(l.toError(msg), isCommitted = true)

}

