package _10_Ref

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

object ConcurrentSharedStateExercise extends IOApp {
  case class User(username: String, age: Int, friends: List[User])

  // use ref to hold the current oldest user
  def findOldest(user: User): IO[User] = {

    def findOldestRec(user: User, ref: Ref[IO, User]): IO[Unit] = {
      val handleUser = ref.update(curr => if (user.age > curr.age) user else curr)
      val handleFriends = user.friends.parTraverse_(user => findOldestRec(user, ref))
      handleUser.both(handleFriends).void
    }

    Ref.of[IO, User](user).flatMap(ref => findOldestRec(user, ref).flatMap(_ => ref.get))
  }


  override def run(args: List[String]): IO[ExitCode] = {
    val a = User("a", 60, Nil)
    val b = User("b", 405, Nil)
    val c = User("c", 75, Nil)
    val d = User("d", 10, List(a, b))
    val e = User("e", 55, List(c))
    val f = User("f", 15, List(d, e))

    findOldest(f).flatTap(user => IO.println(user.age + " " + user.username)).as(ExitCode.Success)
  }
}