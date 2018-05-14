package scheme

import scheme.SchemeUtils.Format._
import scheme.SchemeUtils.FormatSyntax._

import org.scalatest.EitherValues._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import scheme.SchemeUtils._

class SchemeParserCheck extends FlatSpec with Checkers with Matchers {
  val alphaOrSymbol: Gen[Char] = Gen.oneOf(Gen.alphaChar, Gen.oneOf("!#$%&|*+-/:<=>?@^_~"))
  val numChar: Gen[Char] = Gen.chooseNum(0, 9).map(_.toString.charAt(0))

  "SchemeParser" should "parse a Symbol" in {
    val alphaOrSymbol: Gen[Char] = Gen.oneOf(Gen.alphaChar, Gen.oneOf("!#$%&|*+-/:<=>?@^_~"))
    val symbolGen: Gen[String] = (for {
      list <- Gen.containerOf[List, Char](
        Gen.oneOf(alphaOrSymbol, numChar)
      )
      startChar <- alphaOrSymbol
    } yield (startChar +: list).mkString(""))
      .suchThat(s => {
        s.length > 0 &&
          !"-+".contains(s.charAt(0)) &&
          s != "#t" &&
          s != "#f" &&
          s.take(2) != "#\\"
      })

    check(forAll(symbolGen)(symbolName => {
      SchemeParser.parseExpr(symbolName) == Right(Symbol(symbolName))
    }))
  }

  it should "parse a String" in {
    check(forAll((string: String) => {
      val quoted = '"' + string + '"'
      SchemeParser.parseExpr(quoted) == Right(LispString(string))
    }))
  }

  it should "parse a String containing escaped characters" in {
    val escapedCharacters = '"' + "\\\"\\\\\\n\\t\\r" + '"'
    val expected = "\"\\\n\t\r"
    SchemeParser.parseExpr(escapedCharacters).right.value should be(LispString(expected))
  }

  it should "parse a Char" in {
    check(forAll(Gen.oneOf(alphaOrSymbol, numChar))((char: Char) => {
      val quoted = "#\\" + char
      SchemeParser.parseExpr(quoted) == Right(LispChar(char))
    }))
  }

  it should "parse a Char by characterName" in {
    SchemeParser.parseExpr("#\\space") == Right(LispChar(' '))
    SchemeParser.parseExpr("#\\newline") == Right(LispChar('\n'))
  }

  it should "parse a quoted" in {
    val inp = "('a)"
    val expexted = LispList(List(LispList(List(Symbol("quote"), Symbol("a")))))
    SchemeParser.parseExpr(inp).right.value should be(expexted)
  }

  it should "parse a number" in {
    check(forAll((int: Int) => {
      SchemeParser.parseExpr(int.toString) == Right(LispNumber(int.toDouble))
    }))

    check(forAll((double: Double) => {
      SchemeParser.parseExpr(double.toString) == Right(LispNumber(double))
    }))
  }

  // TODO property based test?
  it should "parse a List" in {
    val inp = "(2 2.1 a \n\"a\" )"
    val expected = LispList(List(
      LispNumber(2), LispNumber(2.1), Symbol("a"), LispString("a")
    ))
    SchemeParser.parseExpr(inp).right.value should be(expected)
  }

  it should "parse multiple nested Lists" in {
    val inp = "((1) 2)"
    val expected = LispList(List(
      LispList(List( LispNumber(1))),
      LispNumber(2)
    ))
    SchemeParser.parseExpr(inp).right.value should be(expected)
  }

  // TODO property based test?
  it should "parse a DottedList" in {
    val inp = "(2 2.1 a \n. \"a\" )"
    val expected = DottedLispList(LispList(List(
      LispNumber(2), LispNumber(2.1), Symbol("a")
    )), LispString("a"))
    SchemeParser.parseExpr(inp).right.value should be(expected)
  }

  it should "ignore whitespace before the input" in {
    val inp = "  (1)"
    val expected = LispList(List(LispNumber(1)))
    SchemeParser.parseExpr(inp).right.value should be (expected)
  }
}
