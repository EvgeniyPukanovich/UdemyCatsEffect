package _06_ResourceSafety

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.io._
import java.net.{HttpURLConnection, URL}

object ResourceSafetyExercise extends IOApp {

  def createConnection(targetURL: String): Resource[IO, HttpURLConnection] = {

    def acquireConnection = IO.blocking {
      val connection = new URL(targetURL).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }

    Resource.make(acquireConnection)(conn => IO.blocking(conn.disconnect()))
  }

  def readOutput(reader: BufferedReader): IO[String] =
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }

  def createBufferReader(reader: Reader): Resource[IO, BufferedReader] = {
    def acquireReader = IO.blocking(new BufferedReader(reader))

    Resource.fromAutoCloseable(acquireReader)
  }

  def createInputStreamReader(inputStream: InputStream): Resource[IO, InputStreamReader] = {
    def acquireReader = IO.blocking(new InputStreamReader(inputStream))

    Resource.fromAutoCloseable(acquireReader)
  }

  def httpGet(targetURL: String): IO[String] = {
    val res = for {
      conn <- createConnection(targetURL)
      inpStreamReader <- createInputStreamReader(conn.getInputStream)
      bufferReader <- createBufferReader(inpStreamReader)
    } yield bufferReader

    res.use(reader => readOutput(reader))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    httpGet("https://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
