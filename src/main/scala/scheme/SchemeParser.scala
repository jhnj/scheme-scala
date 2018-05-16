package scheme

import parser.{ParserImplementation, Parsers}

import scala.language.higherKinds
import cats.syntax.either._
import SchemeUtils._
import parser.ParserUtils.Parser


object SchemeParser {
  private def lispParser[Parser[+ _]](P: Parsers[Parser]): Parser[LispVal] = {
    import P._

    def symbol: Parser[String] = oneOf("!#$%&|*/:+-<=>?@^_~").map(_.toString)

    def parseString: Parser[LispString] = escapedQuotedString.map(LispString)

    def parseChar: Parser[LispChar] = {
      val character: Parser[Char] = List(symbol, letter, digits)
        .map(s => s.map(_.charAt(0)))
        .reduce(_ | _)
      val characterName: Parser[Char] =
        string("space").as(' ') | string("newline").as('\n')
      attempt("#\\" *> (character | characterName)).map(LispChar)
    }

    def parseAtom: Parser[LispVal] = {
      for {
        first <- symbol | letter
        rest <- many(symbol | letter | digits)
      } yield {
        val atom = (first +: rest).mkString("")
        atom match {
          case "#t" => LispBool(true)
          case "#f" => LispBool(false)
          case _ => Symbol(atom)
        }
      }
    }

    def parseQuoted: Parser[LispVal] =
      ("'" *> parseExpr).map(r => LispList(List(Symbol("quote"), r)))

    def parseNumber: Parser[LispNumber] =
      double.or(integer.map(_.toDouble)).map(LispNumber)

    def parseList: Parser[LispList] =
      attempt(surround(token("("), ")")(
        token(parseExpr.separator(whitespace1).map(LispList))
      ))

    def parseDottedList =
      attempt(surround(token("("), token(")"))(
        token(parseExpr.separator(whitespace1).map(LispList)) ** token(token(".") *> parseExpr)
      )).map { case (head, tail) => DottedLispList(head, tail) }

    def parseExpr: Parser[LispVal] =
      parseList |
        parseDottedList |
        parseNumber |
        parseChar |
        parseString |
        parseQuoted |
        parseAtom

    whitespace *> parseExpr
  }

  private def readOrThrow[A](parser: Parser[A])(input: String): ThrowsError[A] = {
    ParserImplementation
      .run(parser)(input)
      .leftMap(scheme.SchemeUtils.ParseError)
  }

  def parseExpr(input: String): ThrowsError[LispVal] = {
    val parser: Parser[LispVal] = SchemeParser.lispParser(ParserImplementation)
    readOrThrow(parser)(input)
  }

  def parseExprList(input: String): ThrowsError[List[LispVal]] = {
    val parser: Parser[LispVal] = SchemeParser.lispParser(ParserImplementation)
    import ParserImplementation._
    val listParser = parser.separator(whitespace1)
    readOrThrow(listParser)(input)
  }


  def readExpr(input: String): String = {
    import SchemeUtils.FormatSyntax._
    val lispParser = SchemeParser.lispParser(ParserImplementation)
    ParserImplementation.run(lispParser)(input) match {
      case Right(lispVal) => s"Found value: ${lispVal.format}"
      case Left(err) => s"Got Error:\n$err"
    }
  }
}
