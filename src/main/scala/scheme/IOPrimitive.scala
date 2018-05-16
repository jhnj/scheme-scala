package scheme

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._
import cats.data.EitherT
import cats.syntax.either._
import cats.effect.IO
import scheme.SchemeUtils._
import scheme.SchemeUtils.FormatSyntax._
import scheme.SchemeUtils.Format._
import scheme.Eval.oneParameter

import scala.collection.immutable.HashMap
import scala.util.Try


object IOPrimitive {

  sealed trait Handle {
    def close(): Unit
  }

  case class Input(get: BufferedReader, name: String) extends Handle {
    def close() = get.close()
  }

  case class Output(get: PrintWriter, name: String) extends Handle {
    def close() = get.close()
  }

  val openInputFile: LispIOPrimitive = wrapOpen { file =>
    val reader = new BufferedReader(new FileReader(file))
    Port(Input(reader, file))
  }

  val openOutputFile: LispIOPrimitive = wrapOpen { file =>
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(file)))
    Port(Output(writer, file))
  }

  private def wrapOpen(f: String => LispVal): LispIOPrimitive =
  /*_*/
  // Intellij doesn't understand this
    oneParameter {
      case LispString(file) =>
        EitherT(IO {
          Try(f(file)).toEither.leftMap(_ => Default(s"Could not open file: $file"))
        })
      case lv => leftT(TypeMismatch("string", lv))
    }

  /*_*/

  def closePort(lispVal: LispVal): EitherT[IO, LispError, LispVal] = lispVal match {
    case Port(handle) => EitherT.right(IO {
      handle.close()
      LispBool(true)
    })
    case lv => leftT(TypeMismatch("port", lv))
  }

  def withOnePort(f: Handle => IOThrowsError[LispVal]): LispIOPrimitive =
  /*_*/
  // Intellij doesn't understand this
    oneParameter {
      case Port(handle) => f(handle)
      case lv => leftT(TypeMismatch("port", lv))
    }

  /*_*/

  def read(handle: Handle): IOThrowsError[LispVal] = handle match {
    case Input(input, name) => EitherT(IO {
      Try {
        LispString(input.readLine())
      }.toEither.leftMap(_ => Default(s"Failed to read file: $name"))
    })
    case Output(_, name) => leftT(Default(s"Can't read file: $name opened for writing"))
  }

  def write(toWrite: LispVal, handle: Handle): IOThrowsError[LispVal] = handle match {
    case Output(output, name) =>
      EitherT(IO {
        Try {
          output.println(toWrite.format)
          output.flush()
          LispBool(true)
        }.toEither.leftMap(_ => Default(s"Failed to write file: $name"))
      })

    case Input(_, name) => leftT(Default(s"Can't write to file: $name opened for reading"))
  }

  def stdOut = Output(new PrintWriter(System.out), "stdOut")

  def writeProc(list: List[LispVal]): IOThrowsError[LispVal] = list match {
    case List(toWrite) => write(toWrite, stdOut)
    case List(toWrite, Port(handle)) => write(toWrite, handle)
    case List(_, lv) => leftT(TypeMismatch("port", lv))
    case l => leftT(NumArgs(2, l))
  }

  def readContents(lispVal: LispVal): IOThrowsError[LispVal] = lispVal match {
    case LispString(file) => readFileContents(file).map(LispString)
    case lv => leftT(TypeMismatch("string", lv))
  }


  def readAll(lispVal: LispVal): IOThrowsError[LispVal] = lispVal match {
    case LispString(file) => load(file).map(LispList)
    case lv => leftT(TypeMismatch("string", lv))
  }

  def load(fileName: String): IOThrowsError[List[LispVal]] = for {
    text <- readFileContents(fileName)
    res <- EitherT.fromEither[IO](SchemeParser.parseExprList(text))
  } yield res

  private def readFileContents(fileName: String): IOThrowsError[String] = EitherT(IO {
    Try {
      Files.readAllLines(Paths.get(fileName), StandardCharsets.UTF_8)
    }
      .toEither
      .bimap(_ => Default(s"Reading file: $fileName, failed"), list => list.asScala.mkString("\n"))
  })

  val ioPrimitives: Map[String, LispIOPrimitive] = HashMap(
    "open-input-file" -> openInputFile,
    "open-output-file" -> openOutputFile,
    "close-input-port" -> oneParameter(closePort _),
    "close-output-port" -> oneParameter(closePort _),
    "read" -> withOnePort(read),
    "write" -> writeProc _,
    "read-contents" -> oneParameter(readContents _),
    "read-all" -> oneParameter(readAll _)
  )
}
