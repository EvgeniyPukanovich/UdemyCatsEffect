package _13_ThreadPools

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import java.util.concurrent.LinkedBlockingQueue

// Hints:
// - LinkedBlockingQueue for tasks
// - Workers run forever
object ThreadPoolExercise {

  class FixedThreadPool(noThreads: Int) {

    val queue = new LinkedBlockingQueue[Runnable]()

    case class Worker() extends Thread {
      override def run(): Unit = {
        while (true) {
          try
            queue.take.run()
          catch {
            case e: InterruptedException =>
              println(e.getMessage)
          }
        }
      }
    }

    List.fill(noThreads)(Worker().start())

    def execute(runnable: Runnable): Unit = queue.offer(runnable)
  }

  def main(args: Array[String]): Unit = {
    val pool = new FixedThreadPool(3)

    case class BlockingTask(id: Long) extends Runnable {
      override def run(): Unit = {
        println(s"Running blocking task $id on thread ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println(s"Waking up blocking task $id on thread ${Thread.currentThread().getName}")
      }
    }
    (1 to 6).foreach { i =>
      pool.execute(BlockingTask(i))
    }

  }
}
