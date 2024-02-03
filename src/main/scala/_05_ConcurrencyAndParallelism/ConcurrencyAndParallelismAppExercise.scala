package _05_ConcurrencyAndParallelism

import cats.effect._
import cats.implicits._

import scala.concurrent.duration.DurationInt

object ConcurrencyAndParallelismAppExercise extends IOApp {
  case class Quote(author: String, text: String)

  def fetchHttp(n: Int): IO[List[Quote]] =
    IO.sleep(1000.millis) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchDb(n: Int): IO[List[Quote]] =
    IO.sleep(5000.millis) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchAuthorAge(author: String): IO[Int] =
    IO.sleep(1000.millis) *> IO((math.random() * 100).toInt)


  override def run(args: List[String]): IO[ExitCode] = {
    val n = 5

    // fetch n quotes from the fastest source
    // calculate the average age of the authors
    IO.race(fetchDb(n), fetchHttp(n))
      .map {_.fold(identity,identity)}
      .flatMap(lst => lst.parTraverse(q => fetchAuthorAge(q.author)))
      .flatTap(IO.println)
      .map(lst => lst.sum / lst.length)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
