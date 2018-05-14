package scheme

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.EitherValues._
import org.scalatest.prop.Checkers
import scheme.SchemeUtils._
import scheme.SchemeUtils.FormatSyntax._
import scheme.SchemeUtils.Format._

class EvalCheck extends FlatSpec with Checkers with Matchers {
  implicit def evalIO[A](io: IOThrowsError[A]): Either[LispError, A] = io.value.unsafeRunSync()

  "primitives" should "include +" in {
    Eval().primitives("+")(List(1d, 2d, 3d).map(LispNumber)).right.value should be(LispNumber(6))
  }

  it should "include -" in {
    Eval().primitives("-")(List(3d, 2d, 1d).map(LispNumber)).right.value should be(LispNumber(0))
  }

  it should "include *" in {
    Eval().primitives("*")(List(4d, 3d, 2d).map(LispNumber)).right.value should be(LispNumber(24))
  }

  it should "include /" in {
    Eval().primitives("/")(List(8d, 4d, 2d).map(LispNumber)).right.value should be(LispNumber(1))
  }

  it should "include mod" in {
    Eval().primitives("mod")(List(10d, 7d, 2d).map(LispNumber)).right.value should be(LispNumber(1))
  }

  "primitives" should "return an error with more than 1 params" in {
    val args = List("symbol1", "symbol2").map(Symbol)
    Eval().primitives("symbol?")(args).left.value should be(NumArgs(1, args))
  }

  it should "include symbol?" in {
    Eval().primitives("symbol?")(List(Symbol("symbol1"))).right.value should be(LispBool(true))
    Eval().primitives("symbol?")(List(LispString("string"))).right.value should be(LispBool(false))
  }

  it should "include string?" in {
    Eval().primitives("string?")(List(LispString("string"))).right.value should be(LispBool(true))
    Eval().primitives("string?")(List(Symbol("symbol1"))).right.value should be(LispBool(false))
  }

  it should "include number?" in {
    Eval().primitives("number?")(List(LispNumber(1))).right.value should be(LispBool(true))
    Eval().primitives("number?")(List(Symbol("symbol1"))).right.value should be(LispBool(false))
  }

  "primitives" should "include symbol->string" in {
    Eval().primitives("symbol->string")(List(Symbol("symbol1"))).right.value should be(LispString("symbol1"))
  }

  it should "include string->symbol" in {
    Eval().primitives("string->symbol")(List(LispString("string"))).right.value should be(Symbol("string"))
  }

  "primitives" should "have working number primitives" in {
    Eval().primitives("=")(List(LispNumber(1), LispNumber(1))).right.value should be(LispBool(true))
  }
  it should "have working boolean primitives" in {
    Eval().primitives("&&")(List(LispBool(true), LispBool(false))).right.value should be(LispBool(false))
  }
  it should "have working string primitives" in {
    Eval()
      .primitives("string=?")(List(LispString("string"), LispString("string")))
      .right.value should be(LispBool(true))
  }

  "unpackBoolean" should "unpack a boolean" in {
    Eval().unpackBoolean(LispBool(true)).right.value should be (true)
  }

  it should "should fail when not given a boolean" in {
    val notBoolean = LispString("not a boolean")
    Eval()
      .unpackBoolean(notBoolean)
      .left.value should be (TypeMismatch("boolean", notBoolean))
  }

  "unpackString" should "unpack a string" in {
    val string = "string"
    Eval().unpackString(LispString(string)).right.value should be (string)
  }
  it should "unpack a boolean as string" in {
    val lispBool = LispBool(true)
    Eval().unpackString(lispBool).right.value should be (lispBool.format)
  }

  it should "unpack a number as string" in {
    val lispNumber = LispNumber(1)
    Eval().unpackString(lispNumber).right.value should be (lispNumber.format)
  }

  "if" should "return the evaluated conseq if pred is not false" in {
    val conseq = LispString("conseq")
    val alt = LispString("alt")
    Eval().eval(LispList(List(Symbol("if"), LispString("not false"), conseq, alt)))
      .right.value should be(conseq)
  }

  it should "return the evaluated alt if pred is false" in {
    val conseq = LispString("conseq")
    val alt = LispString("alt")
    Eval().eval(LispList(List(Symbol("if"), LispBool(false), conseq, alt)))
      .right.value should be(alt)
  }

  def carFixture = new {
    val head = LispString("head")
    val tail = LispString("tail")
    val last = LispString("last")
    val list = LispList(List(head, tail))
  }

  "car" should "return the head of a list" in {
    val f = carFixture
    val lispListInput = List(f.list)
    Eval().primitives("car")(lispListInput).right.value should be (f.head)
  }

  it should "return the head of a DottedList" in {
    val f = carFixture
    val dottedLispListInput = List(DottedLispList(f.list, f.last))
    Eval().primitives("car")(dottedLispListInput).right.value should be (f.head)
  }

  it should "take only 1 parameter" in {
    val f = carFixture
    val multipleArgs = List(f.list, f.list)
    Eval().primitives("car")(multipleArgs).left.value should be (NumArgs(1, multipleArgs))
  }

  def cdrFixture = new {
    val head = LispString("head")
    val tail = List(LispString("tail"))
    val list = LispList(head +: tail)
    val last = LispString("last")
  }

  "cdr" should "return the tail of a list" in {
    val f = cdrFixture
    val lispListInput = List(f.list)
    Eval().primitives("cdr")(lispListInput).right.value should be (LispList(f.tail))
  }

  it should "return the tail of a dottedList" in {
    val f = cdrFixture
    val dottedLispListInput = List(DottedLispList(f.list, f.last))
    Eval().primitives("cdr")(dottedLispListInput).right.value should be (LispList(f.tail :+ f.last))
  }

  it should "take only 1 parameter" in {
    val f = cdrFixture
    val multipleArgs = List(DottedLispList(f.list, f.last), DottedLispList(f.list, f.last))
    Eval().primitives("cdr")(multipleArgs).left.value should be (NumArgs(1, multipleArgs))
  }

  def consFixture = new {
    val head = LispString("head")
    val tail = List(LispString("tail"))
    val list = LispList(head +: tail)
    val last = LispString("last")

    val cons = Eval().primitives("cons")
  }

  "cons" should "return a 1 length list when adding Nil" in {
    val f = consFixture
    f.cons(List(f.head, LispList(Nil))).right.value should be (LispList(List(f.head)))
  }

  it should "append to the start of a list" in {
    val f = consFixture
    val params: List[LispVal] = List(f.head, LispList(f.tail))
    f.cons(params).right.value should be (f.list)
  }

  it should "append to the start of the list part of a dottedList" in {
    val f = consFixture
    val dottedList = DottedLispList(LispList(f.tail), f.last)
    val resDottedList = DottedLispList(f.list, f.last)
    f.cons(List(f.head, dottedList)).right.value should be (resDottedList)
  }

  it should "combine 2 non lists to a dottedList" in {
    val f = consFixture
    val resDottedList = DottedLispList(LispList(List(f.head)), f.head)
    f.cons(List(f.head, f.head)).right.value should be (resDottedList)
  }

  it should "create a dottedList if the first parameter is a list" in {
    val f = consFixture
    val resDottedList = DottedLispList(LispList(List(f.list)), f.last)
    f.cons(List(f.list, f.last)).right.value should be (resDottedList)
  }

  it should "not take more than 2 parameters" in {
    val params = List.fill(3)(LispString("param"))
    Eval()
      .primitives("cons")(params)
      .left.value should be (NumArgs(2, params))
  }

  "eqv" should "return true when comparing 2 equal non-list" in {
    val input = LispString("string")
    Eval().primitives("eqv?")(List(input, input)).right.value should be (LispBool(true))
  }

  it should "return false when comparing 2 lists" in {
    val input = LispList(List(LispString("string")))
    Eval()
      .primitives("eqv?")(List(input, input))
      .right.value should be (LispBool(false))
  }

  it should "accept only 2 parameters" in {
    val args = List.fill(3)(LispBool(true))
    Eval().primitives("eqv?")(args).left.value should be (NumArgs(2, args))
  }

  "equal" should "return true when comparing 2 equal dottedLists" in {
    val input = DottedLispList(
      LispList(List(LispString("string"), LispNumber(1))),
      LispBool(true)
    )
    Eval().primitives("equal?")(List(input, input)).right.value should be (LispBool(true))
  }

  it should "return true when comparing 2 lists with elements that differ" in {
    val input1 = LispList(List(LispString("string1")))
    val input2 = LispList(List(LispString("string2")))
    Eval()
      .primitives("equal?")(List(input1, input2))
      .right.value should be (LispBool(false))
  }

  it should "accept only 2 parameters" in {
    val args = List.fill(3)(LispBool(true))
    Eval().primitives("equal?")(args).left.value should be (NumArgs(2, args))
  }

  def condFixture = new {
    def base(clauses: List[LispVal]) = LispList(Symbol("cond") :: clauses)
    def clause(b: Boolean, expressions: List[LispVal]) = LispList(LispBool(b) :: expressions)
    def elseClause(expressions: List[LispVal]) = LispList(Symbol("else") :: expressions)
  }

  "cond" should "choose the first matching condition" in {
    val f = condFixture
    val input = f.base(List(
      f.clause(b=true, List(LispString("first"))),
      f.clause(b=true, List(LispString("noMatch"))),
      f.elseClause(List(LispString("noMatch")))
    ))
    Eval().eval(input).right.value should be(LispString("first"))
  }

  it should "return the result of the last expression" in {
    val f = condFixture
    val input = f.base(List(
      f.clause(true, List(LispString("first"), LispString("second")))
    ))
    Eval().eval(input).right.value should be(LispString("second"))
  }

  it should "return the result from the else clause if no conditions match" in {
    val f = condFixture
    val input = f.base(List(
      f.clause(b=false, List(LispString("first"))),
      f.elseClause(List(LispString("second")))
    ))
    Eval().eval(input).right.value should be(LispString("second"))
  }

  it should "return Undefined if nothing matches" in {
    val f = condFixture
    val input = f.base(List(
      f.clause(b=false, List(LispString("first")))
    ))
    Eval().eval(input).right.value should be(undefined)
  }
}
