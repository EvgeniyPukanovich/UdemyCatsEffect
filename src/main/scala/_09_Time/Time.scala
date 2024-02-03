package _09_Time

import cats.effect.{ExitCode, IO, IOApp}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

object Time extends IOApp {

  case class Token(value: String, expirationTimeInMillis: Long) {
    def isExpired(): IO[Boolean] = {
      IO.realTime.map(_.toMillis > expirationTimeInMillis)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      currentTime <- IO.realTime
      token = Token("123", (currentTime - FiniteDuration(10, SECONDS)).toMillis)
      isExpired <- token.isExpired()
      _ <- IO.println(s"Is expired: $isExpired")
    } yield ExitCode.Success
  }

  def timeIt[A](expr: IO[A]): IO[FiniteDuration] = {
    for {
      start <- IO.monotonic
      _ <- expr
      end <- IO.monotonic
    } yield end - start

  }

  //override def run(args: List[String]): IO[ExitCode] = timeIt(IO.sleep(1555.millis)).flatTap(IO.println) as (ExitCode.Success)

}
