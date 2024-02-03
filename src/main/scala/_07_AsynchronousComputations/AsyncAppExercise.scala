package _07_AsynchronousComputations

import cats.effect._
import cats.implicits._

object AsyncAppExercise extends IOApp {
  case class User(id: Long, username: String)

  type Error = String

  def findUser(id: Long)(cb: Either[Error, User] => Unit): Unit = {
    val rand = math.random()
    if (rand < 0.5) cb(Right(User(id, s"User $id")))
    else cb(Left("Something went wrong: " + rand))
  }

  def findUserIO(id: Long): IO[User] = IO.async_(cb => findUser(id) {
    case Left(value) => cb(Left(new Exception(value)))
    case Right(value) => cb(Right(value))
  })

  override def run(args: List[String]): IO[ExitCode] = {
    findUserIO(5)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}