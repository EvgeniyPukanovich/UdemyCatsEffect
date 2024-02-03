package _13_ThreadPools

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt

object ComputeBlocking extends IOApp {

  def blockingTask(id: Int) = IO.blocking {
    println(s"Running blocking task $id on thread ${Thread.currentThread().getName}")
    Thread.sleep(2000)
    println(s"Waking up blocking task $id on thread ${Thread.currentThread().getName}")
  }

  def computingTask(id: Int) = IO.println(s"Running task $id on thread ${Thread.currentThread().getName}")

  def compute = (1 to 1000).toList.parTraverse_(id => blockingTask(id))

  override def run(args: List[String]): IO[ExitCode] = compute.timeoutTo(5.seconds, IO.unit)as(ExitCode.Success)
}
