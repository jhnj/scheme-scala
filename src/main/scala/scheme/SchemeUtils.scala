package scheme

import cats.data.EitherT
import cats.effect.IO

import IOPrimitive.Handle
import scheme.SchemeUtils.FormatSyntax._
import scheme.SchemeUtils.Format._

object SchemeUtils {
  type LispPrimitive = List[LispVal] => ThrowsError[LispVal]
  type LispIOPrimitive = List[LispVal] => IOThrowsError[LispVal]

  sealed trait LispVal
  case class Symbol(name: String) extends LispVal
  case class LispList(get: List[LispVal]) extends LispVal
  case class DottedLispList(list: List[LispVal], last: LispVal) extends LispVal
  case class LispNumber(get: Double) extends LispVal
  case class LispString(get: String) extends LispVal
  case class LispChar(get: Char) extends LispVal
  case class LispBool(get: Boolean) extends LispVal
  case class PrimitiveFunc(func: LispPrimitive) extends LispVal
  case class IOFunc(func: LispIOPrimitive) extends LispVal
  case class Port(handle: Handle) extends LispVal
  case class Func(params: List[String],
                  vararg: Option[String],
                  body: List[LispVal],
                  closure: Env) extends LispVal
  val undefined: LispVal = LispList(List())


  sealed trait LispError
  case class NumArgs(expected: Int, found: List[LispVal]) extends LispError
  case class TypeMismatch(expected: String, found: LispVal) extends LispError
  case class ParseError(err: parser.ParseError) extends LispError
  case class BadSpecialForm(msg: String, form: LispVal) extends LispError
  case class NotFunction(msg: String, func: String) extends LispError
  case class UnboundVar(msg: String, varName: String) extends LispError
  case class Default(msg: String) extends LispError

  type ThrowsError[A] = Either[LispError, A]
  type IOThrowsError[A] = EitherT[IO, LispError, A]

  def rightT[A](a: A): IOThrowsError[A] = EitherT.fromEither(Right(a))
  def leftT[A](err: LispError): IOThrowsError[A] = EitherT.fromEither(Left(err))

  def liftThrows[A](throwsError: ThrowsError[A]): EitherT[IO, LispError, A] =
    EitherT.fromEither(throwsError)

  def runIOThrows(ioThrows: IOThrowsError[String]): IO[String] =
    ioThrows.value.map {
      case Right(string) => string
      case Left(err) => err.format
    }

  trait Format[A] {
    def format(value: A): String
  }

  object Format {
    implicit def lispValToString[A <: LispVal]: Format[A] = {
      case Symbol(name) => name
      case LispList(List(Symbol("quote"), lispVal)) => lispVal.format
      case LispList(list) =>
        "(" + list.map(_.format).mkString(" ") + ")"
      case DottedLispList(list, last) =>
        "(" + list.map(_.format).mkString(" ") + " . " + last + ")"
      case LispNumber(n) => n.toString
      case LispString(s) => '"' + s + '"'
      case LispChar(c) => "#\\" + c.toString
      case LispBool(b) =>
        if (b) "#t"
        else "#f"
      case _: PrimitiveFunc => "<primitive>"
      case _: IOFunc => "<IO primitive>"
      case _: Port => "<IO port>"
      case Func(params, vararg, body, closure) =>
        "(lambda (" + {params.mkString(" ") +
          vararg.map(" . " + _).getOrElse("") +
          ") ...)"
        }

    }

    implicit def lispErrorToString[A <: LispError]: Format[A] = {
      case UnboundVar(message, varName) => s"$message: $varName"
      case BadSpecialForm(message, form) => s"$message: ${form.format}"
      case NotFunction(message, func) => s"$message: $func"
      case NumArgs(expected, found) =>
        s"Expected $expected args, found values ${found.map(l => l.format).mkString(" ")}"
      case TypeMismatch(expected, found) => s"Invalid type: expected $expected, found ${found.format}"
      case ParseError(parseError) => s"Parse error:\n$parseError"
      case Default(msg) => msg
    }

    implicit val throwsErrorToString: Format[ThrowsError[LispVal]] = {
      case Right(lispVal) => lispVal.format
      case Left(err) => err.format
    }
  }

  object FormatSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Format[A]): String = p.format(value)
    }
  }
}
