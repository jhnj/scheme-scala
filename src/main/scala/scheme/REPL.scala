package scheme

import cats.effect.IO

import scala.io.StdIn
import scheme.SchemeUtils.Format._
import scheme.SchemeUtils.FormatSyntax._
import scheme.SchemeUtils.liftThrows

object REPL {
  def readPrompt(prompt: String) = IO {
    StdIn.readLine(prompt)
  }

  def putStr(string: String) = IO {
    println(string)
  }

  def evalAndPut(input: String, eval: Eval): IO[Unit] = {
    (for {
      parsed <- liftThrows(SchemeParser.parseExpr(input))
      evaluated <- eval.eval(parsed)
    } yield evaluated).value
      .flatMap(either => putStr(either.format))
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
      _ <- eval.addPrimitiveBindings
      _ <- go("Lisp>>>  ")
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    (args.length match {
      case 0 => runRepl
      case 1 => for {
        eval <- Eval.withPrimitiveBindings()
        _ <- evalAndPut(args.head, eval)
      } yield ()
      case _ => putStr("Provide either 0 or 1 arguments")
    }).unsafeRunSync
  }
}
