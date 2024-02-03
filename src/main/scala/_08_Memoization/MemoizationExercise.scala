package _08_Memoization

import cats.effect._
import cats.implicits._

object MemoizationExercise extends IOApp {
  trait Currency

  case object Dollar extends Currency

  case object Euro extends Currency

  case class Balance(amount: Double, currency: Currency)

  // pretend this is calling an API and takes some time
  def fetchDollarExchangeRate(currency: Currency): IO[Double] = IO.blocking {
    Thread.sleep(5000)
    currency match {
      case Dollar => 1.0
      case Euro => 1.12
    }
  }

  def getBalancesInDollars(balances: List[Balance]): IO[List[Double]] = {
    fetchDollarExchangeRate(Euro).memoize.flatMap { euroExchangeRateIO =>
      balances.traverse(balance => balance.currency match {
        case Dollar => IO.pure(balance.amount)
        case Euro => euroExchangeRateIO.map(balance.amount * _)
      })
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchange rate once
    val balances = List(Balance(10, Euro), Balance(20, Euro), Balance(30, Euro))
    getBalancesInDollars(balances).flatTap(lst => IO.println(lst.mkString_(","))).as(ExitCode.Success)
  }
}