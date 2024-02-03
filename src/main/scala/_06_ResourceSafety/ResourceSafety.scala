package _06_ResourceSafety

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxParallelTraverse1}

import java.io.{File, FileInputStream, FileOutputStream}

object ResourceSafety extends IOApp {

  trait RowEncoder[A] {
    def encode(a: A): String
  }

  case class Person(name: String, age: Int)

  implicit val personEncoder = new RowEncoder[Person] {
    override def encode(a: Person): String = s"${a.name},${a.age}"
  }

  def writeAll[A](objects: List[A], file: File)(implicit encoder: RowEncoder[A]): IO[Unit] = {
    val p = new java.io.PrintWriter(file)

    def use(lst: List[String]) = IO.raiseError(new Exception("boom"))

    def release = IO.println("Release executing ") *> IO.blocking(p.close())

    objects.parTraverse(o => IO.pure(encoder.encode(o))).bracket(use)(_ => release)
  }

  def encryptFile(sourceFile: File, destFile: File): IO[Unit] = {

    def write(bytes: Array[Byte], fos: FileOutputStream): IO[Unit] = ???

    def read(fis: FileInputStream): IO[Array[Byte]] = ???

    def encrypt(bytes: Array[Byte]): IO[Array[Byte]] = ???

    def close(ac: AutoCloseable): IO[Unit] = IO.blocking(ac.close())

    val acquireReader = IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.blocking(new FileOutputStream(destFile))
    val readerRes = Resource.make[IO, FileInputStream](acquireReader)(close)
    val writerRes = Resource.make[IO, FileOutputStream](acquireWriter)(close)
    val readerAndWriterRes = readerRes.flatMap(r => writerRes.map(w => (r, w)))

    readerAndWriterRes.use { case (reader, writer) => read(reader).flatMap(encrypt).flatMap(write(_, writer)) }
  }

  override def run(args: List[String]): IO[ExitCode] = writeAll(List(Person("asss", 7), Person("dfdf", 8)), new File("dest.txt"))
    .as(ExitCode.Success)
}

object ResourceSafetyIIApp extends IOApp {
  def write(bytes: Array[Byte], fos: FileOutputStream): IO[Unit] = IO.println("writing") *> IO.blocking(fos.write(bytes))

  def read(fis: FileInputStream): IO[Array[Byte]] =
    IO.println("reading") *>
      IO.blocking {
        Iterator
          .continually(fis.read)
          .takeWhile(_ != -1)
          .map(_.toByte)
          .toArray
      }

  def encrypt(bytes: Array[Byte]): IO[Array[Byte]] = {
    IO.println("encrypting") *> bytes.map(b => (b + 1).toByte).pure[IO]
  }

  def closeWriter(ac: AutoCloseable): IO[Unit] = IO.println("closing writer") *> IO.blocking(ac.close())

  def closeReader(ac: AutoCloseable): IO[Unit] = IO.println("closing reader") *> IO.blocking(ac.close())

  def encryptFile(sourceFile: File, destFile: File): IO[Unit] = {
    val acquireReader = IO.println("acquiring reader") *> IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.println("acquiring writer") *> IO.blocking(new FileOutputStream(destFile))
    acquireReader.bracket { reader =>
      acquireWriter.bracket { writer =>
        IO.println("using") *>
          read(reader).flatMap(encrypt).flatMap(write(_, writer))
      }(closeWriter)
    }(closeReader)
  }

  def encryptFile2(sourceFile: File, destFile: File): IO[Unit] = {
    val acquireReader = IO.println("acquiring reader") *> IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.println("acquiring writer") *> IO.raiseError(new Exception("boom")) *> IO.blocking(new FileOutputStream(destFile))

    val readerRes = Resource.make[IO, FileInputStream](acquireReader)(closeReader)
    val writerRes = Resource.make[IO, FileOutputStream](acquireWriter)(closeWriter)
    val readerAndWriterRes = readerRes.flatMap(r => writerRes.map(w => (r, w)))

    readerAndWriterRes.use { case (reader, writer) => read(reader).flatMap(_ => IO.raiseError(new Exception("boom")).flatMap(encrypt)).flatMap(write(_, writer)) }
  }

  def encryptFile3(sourceFile: File, destFile: File): IO[Unit] = {
    val acquireReader = IO.println("acquiring reader") *> IO.blocking(new FileInputStream(sourceFile))
    val acquireWriter = IO.println("acquiring writer") *> IO.raiseError(new Exception("boom")) *> IO.blocking(new FileOutputStream(destFile))

    val readerRes = Resource.fromAutoCloseable(acquireReader)
    val writerRes = Resource.make[IO, FileOutputStream](acquireWriter)(closeWriter)
    val readerAndWriterRes = readerRes.flatMap(r => writerRes.map(w => (r, w)))

    readerAndWriterRes.use { case (reader, writer) => read(reader).flatMap(_ => IO.raiseError(new Exception("boom")).flatMap(encrypt)).flatMap(write(_, writer)) }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val source = new File("source.txt")
    val dest = new File("dest.txt")
    encryptFile2(source, dest).as(ExitCode.Success)
  }
}

