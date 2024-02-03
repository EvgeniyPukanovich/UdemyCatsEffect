package _11_Deferred

import cats.effect.{Deferred, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel

import scala.concurrent.duration.DurationInt

object DeferredSharedState extends IOApp {

  case class Item(id: Long)

  // Long running
  def loadItems(): IO[List[Item]] = //IO.raiseError(new Exception("sdsdsd"))
    (IO.println("Loading items") *>
      IO.sleep(3.seconds) *>
      IO.println("Items loaded")).as(List(Item(1), Item(2)))

  // Long running
  def initUi(): IO[Unit] =
    IO.println("Initializing UI") *>
      IO.sleep(5.seconds) *>
      IO.println("UI initialized")

  def showItems(items: List[Item]): IO[Unit] = IO.println("Showing items")

  def showError(): IO[Unit] = IO.println("Showing error")

  def setupUiPar(): IO[Unit] =
    (initUi(), loadItems().attempt).parFlatMapN { (_, loadRes) =>
      loadRes match {
        case Left(_) => showError()
        case Right(value) => showItems(value)
      }
    }

  def handleUI(defItems: Deferred[IO, Either[Throwable, List[Item]]]) = initUi() *> defItems.get.flatMap {
    case Left(_) => showError()
    case Right(value) => showItems(value)
  }

  def handleItems(defItems: Deferred[IO, Either[Throwable, List[Item]]]) = loadItems().attempt.flatMap(defItems.complete)

  def setupUiDef() = Deferred[IO, Either[Throwable, List[Item]]].flatMap(deferred => (handleUI(deferred), handleItems(deferred)).parTupled).void

  override def run(args: List[String]): IO[ExitCode] = setupUiDef().as(ExitCode.Success)
}
