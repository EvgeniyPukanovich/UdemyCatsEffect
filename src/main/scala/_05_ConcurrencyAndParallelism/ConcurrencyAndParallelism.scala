package _05_ConcurrencyAndParallelism

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object ConcurrencyAndParallelism1 {
  def run(args: List[String]): IO[ExitCode] = {
    case class Image(bytes: List[Byte])

    def httpImages(n: Int): IO[List[Image]] =
      IO.sleep(5000.millis) *> (1 to n).toList.map(i => Image(List(i.toByte))).pure[IO]

    def dbImages(n: Int): IO[List[Image]] =
      IO.sleep(5000.millis) *> (1 to n).toList.map(i => Image(List((100 + i).toByte))).pure[IO]

    val n = 50

    (httpImages(n), dbImages(n)).parMapN {
      case (httpImages, dbImages) => (httpImages ++ dbImages)
    }.flatTap(IO.println).as(ExitCode.Success)
  }
}

object ConcurrencyAndParallelism2 {
  def run(args: List[String]): IO[ExitCode] = {
    case class Person(name: String)

    def save(person: Person): IO[Long] = IO.sleep(3000.millis) *> person.name.length.toLong.pure[IO]

    val p = Person("asss")
    val p1 = Person("aasdsdsad")
    val lst = List(p1, p, Person("sdsds"))
    lst.parTraverse(p => save(p)).flatTap(IO.println).as(ExitCode.Success)
  }
}

object ConcurrencyAndParallelism3 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    case class Image(bytes: List[Byte])

    def fetchHttp(n: Int): IO[List[Image]] =
      IO.sleep(2000.millis) *> (1 to n).toList.map(i => Image(List(i.toByte))).pure[IO]

    def fetchDb(n: Int): IO[List[Image]] =
      IO.sleep(1200.millis) *> (1 to n).toList.map(i => Image(List((100 + i).toByte))).pure[IO]

    val n = 50

    IO.race(fetchHttp(n),fetchDb(n)) .map {
      case Left(value) => "http won" + value.mkString(",")
      case Right(value) => "db won" + value.mkString(",")
    }.flatTap(IO.println).as(ExitCode.Success)
  }
}