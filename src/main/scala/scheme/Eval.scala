package scheme

import scheme.SchemeUtils._
import cats.instances.either._
import cats.Apply
import cats.syntax.either._
import cats.instances.list._
import cats.instances.option._
import cats.Traverse
import cats.syntax.traverse._
import FormatSyntax._
import Format._
import cats.data.EitherT
import cats.effect.IO

import scala.collection.immutable.HashMap
import scala.util.Try

class Eval(val env: Env) {
  type LispPrimitives = Map[String, List[LispVal] => IOThrowsError[LispVal]]

  def eval(lispVal: LispVal): IOThrowsError[LispVal] = lispVal match {
    case s: LispString => rightT(s)
    case n: LispNumber => rightT(n)
    case b: LispBool => rightT(b)
    case Symbol(id) => env.getVar(id)

    case LispList(List(Symbol("quote"), v)) => rightT(v)

    case LispList(List(Symbol("if"), pred, conseq, alt)) =>
      evalIf(pred, conseq, alt)
    case LispList(Symbol("cond") :: conditions) =>
      evalCond(conditions)
    case LispList(Symbol("case") :: conditions) =>
      liftThrows(evalCase(conditions))

    case LispList(List(Symbol("set!"), Symbol(variable), value)) => for {
      v <- eval(value)
      res <- env.setVar(variable, v)
    } yield res

    case LispList(List(Symbol("define"), Symbol(variable), value)) => for {
      v <- eval(value)
      res <- EitherT.right(env.defineVar(variable, v))
    } yield res

    case LispList(Symbol("define") :: LispList(Symbol(func) :: params) :: body) => for {
      f <- EitherT.fromEither[IO](makeFunc(params, None, body, env))
      v <- EitherT.right(env.defineVar(func, f))
    } yield v

    case LispList(Symbol("define") ::
      DottedLispList(LispList(Symbol(func) :: params), varargs) ::
      body
    ) => for {
      f <- EitherT.fromEither[IO](makeFunc(params, Some(varargs), body, env))
      v <- EitherT.right(env.defineVar(func, f))
    } yield v

    case LispList(Symbol("lambda") :: LispList(params) :: body) =>
      EitherT.fromEither[IO](makeFunc(params, None, body, env))

    case LispList(Symbol("lambda") ::
      DottedLispList(LispList(params), varargs) ::
      body
    ) =>
      EitherT.fromEither[IO](makeFunc(params, Some(varargs), body, env))

    case LispList(Symbol("lambda") :: varargs :: body) =>
      EitherT.fromEither[IO](makeFunc(List(), Some(varargs), body, env))

    case LispList(f :: args) => for {
      func <- eval(f)
      argValues <- args.traverse(eval)
      res <- applyF(func)(argValues)
    } yield res

    case badForm =>
      leftT(BadSpecialForm("Unrecognized special form", badForm))
  }

  def makeFunc(params: List[LispVal],
               vararg: Option[LispVal],
               body: List[LispVal],
               closure: Env): ThrowsError[Func] = {
    /*_*/
    // Intellij doesn't understand traverse here
    val paramsString = params.traverse {
      case Symbol(string) => Right(string)
      case found => Left(TypeMismatch("string", found))
    }

    val varargString = vararg.traverse {
      case Symbol(string) => Right(string)
      case found => Left(TypeMismatch("string", found))
    }
    /*_*/

    for {
      p <- paramsString
      v <- varargString
    } yield Func(p, v, body, closure)
  }

  def evalIf(pred: LispVal, conseq: LispVal, alt: LispVal): IOThrowsError[LispVal] = eval(pred).flatMap {
    case LispBool(false) => eval(alt)
    case _ => eval(conseq)
  }

  def evalCond(conditions: List[LispVal]): IOThrowsError[LispVal] = {
    val clauseOpt: Option[IOThrowsError[LispVal]] = conditions.headOption.map {
      case LispList(Symbol("else") :: expressions) if conditions.length == 1 =>
        evalExpressions(expressions)
      case LispList(test :: Symbol("=>") :: expressions) =>
        ??? // TODO http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1
      case LispList(test :: expressions) => eval(test).flatMap {
        case LispBool(false) => evalCond(conditions.tail)
        case _ => evalExpressions(expressions)
      }
      case badForm => leftT(BadSpecialForm("Invalid cond clause", badForm))
    }

    clauseOpt.getOrElse(rightT(undefined))
  }

  def evalExpressions(expressions: List[LispVal]): IOThrowsError[LispVal] = {
    /*_*/
    // Intellij doesn't understand traverse here
    expressions.traverse(eval).map(_.lastOption.getOrElse(undefined))
    /*_*/
  }

  def evalCase(conditions: List[LispVal]): ThrowsError[LispVal] =
    ??? // TODO http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1

  def applyFunction(func: String)(args: List[LispVal]): IOThrowsError[LispVal] = {
    primitives.get(func).map(_ (args)).getOrElse(leftT(NotFunction("Primitive function not found", func)))
  }

  def applyF(func: LispVal)(args: List[LispVal]): IOThrowsError[LispVal] = func match {
    case PrimitiveFunc(f) => f(args)
    case Func(params, vararg, body, closure) => {
      if (params.length != args.length && vararg.isEmpty)
        leftT(NumArgs(params.length, args))
      else {
        for {
          _ <- EitherT.right(closure.bindVars(params.zip(args)))
          _ <- vararg match {
            case None => EitherT.right(IO.pure(()))
            case Some(argName) =>
              EitherT.right(closure.defineVar(
                argName,
                LispList(args.drop(params.length))
              ))
          }
          res <- {
            val newEnv = Eval(closure)
            body.map(newEnv.eval).last
          }
        } yield res
      }
    }
    case _ => leftT(BadSpecialForm("applyF", func))
  }

  def addPrimitiveBindings: IO[List[LispVal]] =
    env.bindVars(primitives.mapValues(PrimitiveFunc).toList)

  val numericPrimitives: LispPrimitives = HashMap(
    "+" -> numericOp(_ + _),
    "-" -> numericOp(_ - _),
    "*" -> numericOp(_ * _),
    "/" -> numericOp(_ / _),
    "mod" -> numericOp(_ % _)
  )

  def numericOp(op: (Double, Double) => Double)(params: List[LispVal]): IOThrowsError[LispVal] = {
    if (params.length < 2)
      leftT(NumArgs(2, params))
    else
      params
        .map(unpackNumber)
        .reduce(Apply[IOThrowsError].map2(_, _)(op))
        .map(LispNumber)
  }

  def unpackNumber(lispVal: LispVal): IOThrowsError[Double] = eval(lispVal).flatMap {
    case LispNumber(i) => rightT(i)
    case LispString(string) =>
      liftThrows(Try(string.toDouble).toEither.leftMap(_ => TypeMismatch("number", LispString(string))))
    case LispList(List(elem)) => unpackNumber(elem)
    case notNumber => leftT(TypeMismatch("number", notNumber))
  }

  def oneParameter(func: LispVal => ThrowsError[LispVal])(list: List[LispVal]): IOThrowsError[LispVal] = liftThrows {
    if (list.length == 1)
      func(list.head)
    else
      Left(NumArgs(1, list))
  }

  def symbolCheck(lispVal: LispVal): LispVal = lispVal match {
    case Symbol(_) => LispBool(true)
    case _ => LispBool(false)
  }

  def stringCheck(lispVal: LispVal): LispVal = lispVal match {
    case LispString(_) => LispBool(true)
    case _ => LispBool(false)
  }

  def numberCheck(lispVal: LispVal): LispVal = lispVal match {
    case LispNumber(_) => LispBool(true)
    case _ => LispBool(false)
  }

  val typeTestingPrimitives: LispPrimitives = HashMap(
    "symbol?" -> oneParameter((symbolCheck _).andThen(Right(_))),
    "string?" -> oneParameter((stringCheck _).andThen(Right(_))),
    "number?" -> oneParameter((numberCheck _).andThen(Right(_)))
  )

  def symbolString(list: List[LispVal]): ThrowsError[LispVal] = list match {
    case l: List[LispVal] if l.length != 1 => Left(NumArgs(1, list))
    case List(Symbol(symbol)) => Right(LispString(symbol))
    case List(lispVal) => Left(TypeMismatch("Symbol", lispVal))
  }

  def stringSymbol(list: List[LispVal]): ThrowsError[LispVal] = list match {
    case l: List[LispVal] if l.length != 1 => Left(NumArgs(1, list))
    case List(LispString(string)) => Right(Symbol(string))
    case List(lispVal) => Left(TypeMismatch("Symbol", lispVal))
  }

  val symbolPrimitives: LispPrimitives = HashMap(
    "symbol->string" -> (symbolString _).andThen(liftThrows),
    "string->symbol" -> (stringSymbol _).andThen(liftThrows)
  )

  def boolBinOp[A](unpacker: LispVal => IOThrowsError[A])
                  (op: (A, A) => Boolean)
                  (args: List[LispVal]): IOThrowsError[LispVal] = {
    if (args.length != 2)
      leftT(NumArgs(2, args))
    else {
      for {
        a <- unpacker(args.head)
        b <- unpacker(args.last)
      } yield LispBool(op(a, b))
    }
  }

  def unpackBoolean(lispVal: LispVal): IOThrowsError[Boolean] = eval(lispVal).flatMap {
    case LispBool(boolean) => rightT(boolean)
    case _ => leftT(TypeMismatch("boolean", lispVal))
  }

  def unpackString(lispVal: LispVal): IOThrowsError[String] = eval(lispVal).flatMap {
    case LispString(s) => rightT(s)
    case n: LispNumber => rightT(n.format)
    case b: LispBool => rightT(b.format)
    case _ => leftT(TypeMismatch("string", lispVal))
  }

  type BoolBinOp[A] = ((A, A) => Boolean) => List[LispVal] => IOThrowsError[LispVal]

  val numberBoolBinOp: BoolBinOp[Double] = boolBinOp(unpackNumber)
  val booleanBoolBinOp: BoolBinOp[Boolean] = boolBinOp(unpackBoolean)
  val stringBoolBinOp: BoolBinOp[String] = boolBinOp(unpackString)

  val booleanPrimitives: LispPrimitives = HashMap(
    "=" -> numberBoolBinOp(_ == _),
    "<" -> numberBoolBinOp(_ < _),
    ">" -> numberBoolBinOp(_ > _),
    "/=" -> numberBoolBinOp(_ != _),
    "<=" -> numberBoolBinOp(_ <= _),
    ">=" -> numberBoolBinOp(_ >= _),
    "&&" -> booleanBoolBinOp(_ && _),
    "||" -> booleanBoolBinOp(_ || _),
    "string=?" -> stringBoolBinOp(_ == _),
    "string<?" -> stringBoolBinOp(_ < _),
    "string>?" -> stringBoolBinOp(_ > _),
    "string<=?" -> stringBoolBinOp(_ <= _),
    "string>=?" -> stringBoolBinOp(_ >= _),
  )

  def car(lispVal: LispVal): ThrowsError[LispVal] = lispVal match {
    case LispList(head :: _) => Right(head)
    case DottedLispList(LispList(head :: _), _) => Right(head)
    case badArg => Left(TypeMismatch("pair", badArg))
  }

  def cdr(lispVal: LispVal): ThrowsError[LispVal] = lispVal match {
    case LispList(_ :: tail) => Right(LispList(tail))
    case DottedLispList(LispList(List(_)), last) => Right(LispList(List(last)))
    case DottedLispList(LispList(_ :: tail), last) => Right(LispList(tail :+ last))
    case badArg => Left(TypeMismatch("pair", badArg))
  }

  def cons(list: List[LispVal]): ThrowsError[LispVal] = {
    if (list.length != 2)
      Left(NumArgs(2, list))
    else {
      Right(
        list match {
          case List(head, LispList(List(Symbol("quote"), LispList(List())))) =>
            LispList(List(head))
          case List(head, LispList(tail)) => LispList(head +: tail)
          case List(head, DottedLispList(listPart, last)) =>
            DottedLispList(LispList(head +: listPart.get), last)
          case List(head, last) => DottedLispList(LispList(List(head)), last)
        }
      )
    }
  }

  def isList(lispVal: LispVal): ThrowsError[LispVal] = Right(LispBool(
    lispVal match {
      case LispList(head :: tail) => true
      case _ => false
    }
  ))

  def isNull(lispVal: LispVal): ThrowsError[LispVal] = Right(LispBool(
    lispVal match {
      case LispList(List()) => true
      case _ => false
    }
  ))

  val listPrimitives: LispPrimitives = HashMap(
    "car" -> oneParameter(car),
    "cdr" -> oneParameter(cdr),
    "cons" -> (cons _).andThen(liftThrows),
    "list?" -> oneParameter(isList),
    "null?" -> oneParameter(isNull)
  )

  def equalityOp(op: (LispVal, LispVal) => ThrowsError[Boolean])(args: List[LispVal]): ThrowsError[LispBool] = {
    if (args.length != 2)
      Left(NumArgs(2, args))
    else
      op(args.head, args.last).map(LispBool)
  }

  def numericEqual(a: LispVal, b: LispVal): ThrowsError[Boolean] = Right(
    (a, b) match {
      case (LispNumber(n1), LispNumber(n2)) => n1 == n2
      case _ => false
    }
  )

  def eqv(a: LispVal, b: LispVal): ThrowsError[Boolean] =
    numericEqual(a, b).map(numeric => {
      numeric || ((a, b) match {
        case (LispBool(b1), LispBool(b2)) => b1 && b2
        case (LispString(s1), LispString(s2)) => s1 == s2
        case (Symbol(s1), Symbol(s2)) => s1 == s2
        case _ => false
      })
    })

  def equal(a: LispVal, b: LispVal): ThrowsError[Boolean] =
    for {
      shallow <- eqv(a, b)
      nested <- (a, b) match {
        case (DottedLispList(l1, last1), DottedLispList(l2, last2)) =>
          equal(LispList(l1.get :+ last1), LispList(l2.get :+ last2))
        case (LispList(l1), LispList(l2)) =>
          Traverse[List].sequence(l1.zip(l2).map(t => equal(t._1, t._2)))
            .map(list => l1.length == l2.length && list.forall(identity))
        case _ => Right(false)
      }
    } yield shallow || nested

  val equalityPrimitives: LispPrimitives = HashMap(
    "=" -> (equalityOp(numericEqual) _).andThen(liftThrows),
    "eqv?" -> (equalityOp(eqv) _).andThen(liftThrows),
    "equal?" -> (equalityOp(equal) _).andThen(liftThrows)
  )


  val charPrimitives: LispPrimitives = HashMap(
    // TODO http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
  )

  val stringPrimitives: LispPrimitives = HashMap(
    // TODO http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.5
  )

  val primitives: LispPrimitives =
    numericPrimitives ++
      typeTestingPrimitives ++
      symbolPrimitives ++
      booleanPrimitives ++
      listPrimitives ++
      equalityPrimitives

}

object Eval {
  def apply(): Eval = new Eval(Env())

  def apply(env: Env): Eval = new Eval(env)

  def withPrimitiveBindings(): IO[Eval] = withPrimitiveBindings(Env())

  def withPrimitiveBindings(env: Env): IO[Eval] = {
    for {
      eval <- IO.pure(Eval(env))
      _ <- eval.addPrimitiveBindings
    } yield eval
  }
}
