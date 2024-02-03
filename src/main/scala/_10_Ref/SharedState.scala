package _10_Ref

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object SharedState extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = List(5L, 10L, 15L, 20L)
    .parTraverse { id =>
      Ref.of[IO, List[String]](Nil).flatMap {
        implicit ref => loadCustomer(id).flatTap(_ => ref.get.flatTap(lst => IO.println(lst.mkString(";"))))
      }
    }
    .as(ExitCode.Success)

  case class Account()

  case class Purchase()

  case class Activity()

  case class Customer(
                       name: String,
                       accounts: List[Account],
                       purchases: List[Purchase],
                       activities: List[Activity]
                     )

  def loadName(id: Long)(implicit ref: Ref[IO, List[String]]): IO[String] =
    IO.sleep(3000.millis) *>
      ref.update(log => s"Loading name for customer $id" :: log) *>
      IO.pure(s"Customer $id")

  def loadAccounts(id: Long)(implicit ref: Ref[IO, List[String]]): IO[List[Account]] =
    ref.update(log => s"Loading accounts for customer $id" :: log) *>
      IO.pure(List(Account()))

  def loadActivities(id: Long)(implicit ref: Ref[IO, List[String]]): IO[List[Activity]] =
    ref.update(log => s"Loading activities for customer $id" :: log) *>
      IO.pure(List(Activity()))

  def loadPurchases(id: Long)(implicit ref: Ref[IO, List[String]]): IO[List[Purchase]] =
    ref.update(log => s"Loading purchases for customer $id" :: log) *>
      IO.pure(List(Purchase()))

  def loadCustomer(customerId: Long)(implicit ref: Ref[IO, List[String]]): IO[Customer] = {
    (loadName(customerId),
      loadAccounts(customerId),
      loadPurchases(customerId),
      loadActivities(customerId)).parMapN((name, accounts, purchases, activities)
    => Customer(name, accounts, purchases, activities))
      .flatTap(_ => IO.println("customer loaded" + customerId))
  }

}
