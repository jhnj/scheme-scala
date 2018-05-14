package scheme

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.EitherValues._
import scheme.SchemeUtils._
import cats.instances.either._
import cats.Applicative

class EnvCheck extends FlatSpec with Matchers {
  def run[A](io: IOThrowsError[A]): Either[SchemeUtils.LispError, A] = io.value.unsafeRunSync()
  val lispVal = LispString("string")

  "isBound" should "return true if variable is bound" in {
    val e = Env(Map("v" -> lispVal))
    e.isBound("v").unsafeRunSync() should be(true)
  }

  it should "return false if variable is not bound" in {
    val e = Env()
    e.isBound("v").unsafeRunSync() should be(false)
  }

  "getVar" should "return a var if it is bound" in {
    val e = Env(Map("v" -> lispVal))
    run(e.getVar("v")).right.value should be(lispVal)
  }

  it should "return an error if the variable is not bound" in {
    val e = Env()
    run(e.getVar("v"))
      .left.value should be(UnboundVar("Trying to get unbound variable", "v"))
  }

  "setVar" should "set a variable if it is defined" in {
    val e = Env(Map("v" -> LispString("initial")))
    run(for {
      _ <- e.setVar("v", lispVal)
      v <- e.getVar("v")
    } yield v).right.value should be(lispVal)
  }

  it should "return an error if the variable is not bound" in {
    val e = Env()
    run(e.setVar("v", lispVal))
      .left.value should be(UnboundVar("Trying to set unbound variable", "v"))
  }

  "defineVar" should "define a variable" in {
    val e = Env(Map("v" -> LispString("initial")))
    (for {
      _ <- e.defineVar("a", lispVal)
      _ <- e.defineVar("b", lispVal)
      a <- e.getVar("a").value
      b <- e.getVar("b").value
    } yield Applicative[ThrowsError].product(a, b)).unsafeRunSync()
      .right.value should be((lispVal, lispVal))
  }

    "bindVars" should "define multiple variables" in {
      val e = Env(Map("v" -> LispString("initial")))
      (for {
        _ <- e.bindVars(List(("a", lispVal), ("b", lispVal)))
        a <- e.getVar("a").value
        b <- e.getVar("b").value
      } yield Applicative[ThrowsError].product(a, b)).unsafeRunSync()
        .right.value should be((lispVal, lispVal))
  }
}
