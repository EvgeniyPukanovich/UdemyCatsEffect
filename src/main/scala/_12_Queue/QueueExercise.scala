package _12_Queue

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std._

import scala.concurrent.duration.DurationInt
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File
import java.awt.color.ColorSpace
import java.awt.image.ColorConvertOp

object QueueExercise extends IOApp {
  case class ImageInfo(filepath: String, image: BufferedImage)

  def processImage(imageInfo: ImageInfo): ImageInfo = {
    val colorOp = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
    val processedImage = colorOp.filter(imageInfo.image, imageInfo.image)
    imageInfo.copy(image = processedImage)
  }

  def saveImage(image: ImageInfo): IO[Unit] = {
    IO.blocking {
      val fp = image.filepath
      val newPath = s"${fp.substring(0, fp.length - 4)}_processed.jpg"
      ImageIO.write(image.image, "jpg", new File(s"$newPath"))
    }.void
  }

  // please make sure directory exists
  def loadImages(directory: String): IO[List[ImageInfo]] = {
    for {
      dir    <- IO.blocking(new File(directory))
      files  <- IO.blocking(dir.listFiles.toList.filter(f => f.isFile && f.getName.endsWith(".jpg")))
      images <- files.parTraverse(f => IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f))))
    } yield images
  }

  // TODO: Take a processed image from the and save it to the corresponding file
  def imageSaver(
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = processedImageQueue.take.flatMap(saveImage).foreverM

  // TODO: Take a raw image from the queue, process it and put it in the processed queue
  def imageProcessor(
    rawImageQueue: Queue[IO, ImageInfo],
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = rawImageQueue.take.map(processImage).flatMap(im => processedImageQueue.offer(im)).foreverM

  // TODO: Load images from the dir and put them in the queue
  def imageLoader(
    srcDirectory: String,
    rawImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = {
    for{
      images <- loadImages(srcDirectory).handleErrorWith(e => IO.println(e.getMessage).map(_ => List.empty[ImageInfo]))
      x <- images.traverse(im => rawImageQueue.offer(im))
      _ <- IO.pure(x)
    } yield ()
  }

  // TODO: Create the loaders, savers and processors and get them all running!
  def start(
    sourceDirs: List[String],
    noProcessors: Int,
    noSavers: Int
  ): IO[Unit] = {
    for{
      rawImageQueue <- Queue.unbounded[IO, ImageInfo]
      processedImageQueue <- Queue.unbounded[IO, ImageInfo]
      processors = for (_ <- 1 to noProcessors) yield {
        imageProcessor(rawImageQueue, processedImageQueue)
      }
      savers = for (_ <- 1 to noSavers) yield {
        imageSaver(processedImageQueue)
      }
      loaders = sourceDirs.map(x => imageLoader(x,rawImageQueue))
      _ <- (loaders ++ savers.toList ++ processors.toList).parSequence_
    } yield {}
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val dirs = List("kittens", "puppies")
    start(dirs,4,2).timeoutTo(10.seconds, IO.unit).handleErrorWith(e => IO.println(e.getMessage)).as(ExitCode.Success)
  }
}