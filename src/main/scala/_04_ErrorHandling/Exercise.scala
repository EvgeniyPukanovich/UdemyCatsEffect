package _04_ErrorHandling

import cats.data.Validated.Invalid
import cats.data.{Validated, ValidatedNec}
import cats.effect._
import cats.implicits._

import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.control.NonFatal

object ErrorHandlingAppExercise extends IOApp {
  object Validations {
    type Valid[A] = ValidatedNec[String, A]

    def validateExtension(filename: String): Valid[String] =
      Validated.condNec(
        filename.endsWith(".txt"),
        filename,
        s"Invalid extension for file $filename. Only txt files allowed."
      )

    def validateLength(expectedLength: Int, s: String): Valid[String] =
      Validated.condNec(
        s.length <= expectedLength,
        s,
        s"The string $s is over $expectedLength characters long"
      )

    def validateFileName(filename: String): Valid[String] =
      (validateExtension(filename), validateLength(0, filename)).mapN { case (_, s) => s }
  }

  object Service {

    sealed trait DomainError

    case class TextFileNotFound(filename: String) extends DomainError

    def countWords(contents: String): Int =
      contents.split("\\W+").length

    def loadFile(filename: String): IO[Either[DomainError, String]] = {
      def loadFileContents(filename: String): IO[Array[Byte]] = {
        IO.blocking(new FileInputStream(filename))
          .bracket { fis =>
            IO.blocking(
              Iterator
                .continually(fis.read)
                .takeWhile(_ != -1)
                .map(_.toByte)
                .toArray
            )
          } { fis =>
            IO.blocking(fis.close())
          }
      }

      /* 1 */
      // Implement a load file function that loads all the contents of a file into a String
      // If the file does not exist, capture that with the domain error TextFileNotFound
      if (!Files.exists(Paths.get(filename)))
        IO(Left(TextFileNotFound(filename)))
      else
        loadFileContents(filename).map(arr => Right(new String(arr, StandardCharsets.UTF_8)))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    import Service._
    import Validations._
    /* 2 */

    // Use args(0) as the filename to load (assume it is always provided)
    // Validate the filename, and output any problem via the console
    // If loaded successfully, output the number of words in the file (use Service.countWords)
    // If a domain error occurs, communicate it to the user via the console
    // If a technical, nonfatal error occurs, output "Something went wrong" to the console
    // If a fatal error occurs, just reraise it and let everything fail
    val result = validateFileName(args.head) match {
      case Validated.Valid(a) =>  loadFile(a).flatMap {
        case Left(value) => IO.println(value.toString).map(_ => ExitCode.Error)
        case Right(value) => IO.println(Service.countWords(value)).map(_ => ExitCode.Success)
      }
      case Invalid(e) => IO.println(e.mkString_(", ")).map(_ => ExitCode.Error)
    }

    result.handleErrorWith{
      case NonFatal(e) => IO.println(s"Something went wrong: ${e.toString}").map(_ => ExitCode.Error)
      case e: Throwable => IO.raiseError(e)
    }
  }

}