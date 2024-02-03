package _13_ThreadPools

import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.concurrent.ExecutionContext

object WorkStealingPool {
  def main(args: Array[String]): Unit = {

    case class BlockingTask(id: Long) extends Runnable {
      override def run(): Unit = {
        println(s"Running blocking task $id on thread ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println(s"Waking up blocking task $id on thread ${Thread.currentThread().getName}")
      }
    }

    val pool = Executors.newWorkStealingPool()
    //Executors.newCachedThreadPool()
    val executionContext = ExecutionContext.fromExecutorService(pool)
    (1 to 1000).foreach { i =>
      executionContext.submit(BlockingTask(i))
    }
    executionContext.awaitTermination(3L, TimeUnit.SECONDS)
    executionContext.shutdown()
  }
}
