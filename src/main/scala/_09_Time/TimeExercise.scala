package _09_Time

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.time.{Duration, Instant, LocalDateTime, ZoneId}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object TimeExercise extends IOApp {
  def tomorrow(): IO[FiniteDuration] = IO.realTime.map(_ + 1.day)

  def tomorrowDateTime(): IO[LocalDateTime] =
    tomorrow().map(fd => LocalDateTime.ofInstant(Instant.ofEpochMilli(fd.toMillis), ZoneId.systemDefault()))

  override def run(args: List[String]): IO[ExitCode] = {
    tomorrowDateTime().flatTap(IO.println(_)) *>
      IO.pure(ExitCode.Success)
  }
}