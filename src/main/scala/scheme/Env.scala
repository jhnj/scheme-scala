package scheme

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.traverse._

import scheme.SchemeUtils._

import scala.collection.mutable
import scala.collection.immutable

class Env(private val env: mutable.Map[String, LispVal] = mutable.HashMap()) {

  def isBound(variable: String): IO[Boolean] = IO(env.isDefinedAt(variable))

  def getVar(variable: String): IOThrowsError[LispVal] = liftThrows {
    Either.fromOption(env.get(variable), UnboundVar("Trying to get unbound variable", variable))
  }

  def setVar(variable: String, value: LispVal): IOThrowsError[LispVal] = EitherT(IO {
    if (env.isDefinedAt(variable)) {
      env.update(variable, value)
      Right(value)
    } else
      Left(UnboundVar("Trying to set unbound variable", variable))
  })

  def defineVar(variable: String, value: LispVal): IO[LispVal] = IO {
    env.update(variable, value)
    value
  }

  def bindVars(variables: List[(String, LispVal)]): IO[List[LispVal]] =
    variables
      .map { case (s, v) => defineVar(s, v) }
      .sequence


  def g = env
}

object Env {

  def apply(): Env = new Env()

  def apply(env: immutable.Map[String, LispVal]): Env = new Env(mutable.HashMap() ++ env)

}

