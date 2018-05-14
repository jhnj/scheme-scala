package parser

object ParserUtils {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonMatching(input: String, offset: Int, s2: String): Int = {
    var i = 0
    while (i + offset < input.length && i < s2.length) {
      if (input.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (input.length - offset >= s2.length) -1
    else input.length - offset
  }
}

