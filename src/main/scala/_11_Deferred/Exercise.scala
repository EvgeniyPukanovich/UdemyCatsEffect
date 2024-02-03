package _11_Deferred

import cats.effect.{Deferred, ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object Exercise extends IOApp {

  class Producer[A](name: String, deferred: Deferred[IO, A], exec: IO[A]) {
    def run(): IO[Unit] = IO.sleep(3.seconds) *> exec.flatMap(res => deferred.complete(res).flatMap(_ => IO.println(s"$name produced value of $res")))
  }

  class Consumer[A](name: String, deferred: Deferred[IO, A], consume: A => IO[Unit]) {
    def run(): IO[Unit] = IO.sleep(1.seconds) *> deferred.get.flatMap(v => consume(v).flatTap(_ => IO.println(s"$name consumed value of $v")))
  }

  override def run(args: List[String]): IO[ExitCode] = Deferred[IO, Int].flatMap(deferred =>
    (new Producer[Int]("p1", deferred, IO.pure(1)).run(),
      new Producer[Int]("p2", deferred, IO.pure(2)).run(),
      new Producer[Int]("p2", deferred, IO.pure(3)).run(),
      new Consumer[Int]("c1", deferred, i => IO.pure(i * 2)).run(),
      new Consumer[Int]("c2", deferred, i => IO.pure(i * 2)).run()
      ).parTupled.as(ExitCode.Success)
  )
}
