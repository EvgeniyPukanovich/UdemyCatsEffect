package _07_AsynchronousComputations

import cats.effect._
import cats.implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object AsynchronousComputations extends IOApp {
  case class Person(id: String, name: String)

  def findPersonById(id: String)(implicit ec: ExecutionContext): Future[Person] = Future {
    println(s"Thread: ${Thread.currentThread().getName}")
    Person(id, "Random_name")
  }

  def findPersonByIdI0(id: String): IO[Person] = {
    implicit val ec: ExecutionContext = ExecutionContext.global
    IO.fromFuture(IO(findPersonById(id)))
  }

  override def run(args: List[String]): IO[ExitCode] =
    findPersonByIdI0("123")
      .flatTap(IO.println)
      .as(ExitCode.Success)
}
