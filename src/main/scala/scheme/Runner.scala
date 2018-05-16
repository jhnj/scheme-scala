package scheme

import cats.data.EitherT
import cats.effect.IO

import scala.io.StdIn
import scheme.SchemeUtils.Format._
import scheme.SchemeUtils.FormatSyntax._
import scheme.SchemeUtils._

object Runner {
  def readPrompt(prompt: String) = IO {
    StdIn.readLine(prompt)
  }

  def putStr[A](a: A)(implicit format: Format[A]) = IO {
    println(a.format)
  }

  def putErr[A](a: A)(implicit format: Format[A]) = IO {
    System.err.println(a.format)
  }

  def evalAndPut(input: String, eval: Eval): IO[Unit] = {
    (for {
      parsed <- liftThrows(SchemeParser.parseExpr(input))
      evaluated <- eval.eval(parsed)
    } yield evaluated).value
      .flatMap(either => putStr(either))
  }


  def runRepl: IO[Unit] = {
    val eval = Eval()

    def go(prompt: String): IO[Unit] = {
      val input = readPrompt(prompt)
      input.flatMap {
        case "quit" => IO()
        case s => evalAndPut(s, eval).flatMap(_ => go(prompt))
      }
    }

    for {
      _ <- eval.addBindings
      _ <- go("Lisp>>>  ")
    } yield ()
  }

  def runFile(file: String, vars: List[String]): IO[Unit] = {
    val loadFile = LispList(List(Symbol("load"), LispString(file)))
    val eval = Eval(Env())
    val output: IOThrowsError[LispVal] = for {
      _ <- EitherT.right(eval.addBindings)
      _ <- EitherT.right(eval.env.bindVars(List(("args", LispList(vars.map(LispString))))))
      res <- eval.eval(loadFile)
    } yield res match {
      case LispList(list) => list.last
      case _ => undefined
    }

    output.value.flatMap {
      case Right(lispVal) => putStr(lispVal)
      case Left(error) => putErr(error)
    }
  }

  def mainIO(args: List[String]): IO[Unit] =
    if (args.isEmpty)
      runRepl
    else {
      runFile(args.head, args.tail)
    }

  def main(args: Array[String]): Unit = {
    mainIO(args.toList).unsafeRunSync
  }
}
