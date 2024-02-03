package _12_Queue

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object QueueApp extends IOApp {

  sealed trait Event

  case class UserAdded(id: Long) extends Event

  case class UserDeleted(id: Long) extends Event

  def producer(queue: Queue[IO, Event]): IO[Nothing] = {
    val generateEvent: IO[Event] = IO {
      val id = (math.random() * 1000).toLong
      if (math.random() < 0.5) UserAdded(id)
      else UserDeleted(id)
    }
    (IO.sleep(400.millis) *> generateEvent.flatTap(e => IO.println(s"Produced ${e.toString}")).flatMap(queue.offer)).foreverM
  }

  def consumer(queue: Queue[IO, Event]): IO[Nothing] = {
    (IO.sleep(1000.millis) *> queue.take.flatMap(IO.println)).foreverM
  }

  override def run(args: List[String]): IO[ExitCode] =
    for{
      queue <- Queue.bounded[IO,Event](5)
      _ <- (producer(queue),consumer(queue)).parTupled
    } yield ExitCode.Success
}
