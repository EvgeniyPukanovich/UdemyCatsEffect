package _03_TheIOMonad

import cats.effect._

import scala.io.StdIn

object MyApp extends IOApp {
  object Console {
    def putStrLn(s: String): IO[Unit] = IO(println(s))
    def readLine(text: String): IO[String] = IO(StdIn.readLine(text))
  }

  import Console._
  // read a line
  // output the line
  // repeat
  def echoForever: IO[Nothing] = readLine("ssd").flatMap(s => putStrLn(s)).foreverM

  override def run(args: List[String]): IO[ExitCode] = {
    echoForever.as(ExitCode.Success)
  }
}
