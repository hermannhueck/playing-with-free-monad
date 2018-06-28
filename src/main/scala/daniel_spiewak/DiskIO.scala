package daniel_spiewak

import cats.Id

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object UncoolIO {

  import java.io.{File, FileInputStream, InputStream, OutputStream, FileOutputStream}

  def read(file: String): Array[Byte] = {
    var fis: InputStream = null
    try {
      fis = new FileInputStream(file)
      val bytes = new Array[Byte](10 * 1024)
      val n = fis.read(bytes)
      bytes.take(n)
    } finally {
      fis.close()
    }
  }

  def write(file: String, content: Array[Byte]): Unit = {
    var fos: OutputStream = null
    try {
      fos = new FileOutputStream(file)
      fos.write(content)
    } finally {
      fos.close()
    }
  }

  def delete(file: String): Boolean = new File(file).delete()

  def doUncoolIO(): Unit = {
    val foo = read("foo.txt")
    val bar = read("bar.txt")
    write("foobar.txt", foo ++ bar)
  }
}

object dsl {

  sealed trait DiskIO[A]
  final case class Read(file: String) extends DiskIO[Array[Byte]]
  final case class Write(file: String, content: Array[Byte]) extends DiskIO[Unit]
  final case class Delete(file: String) extends DiskIO[Boolean]
}

object interpreters {

  import dsl._

  val idInterpreter: DiskIO ~> Id = new (DiskIO ~> Id) {

    import UncoolIO._

    override def apply[A](fa: DiskIO[A]): Id[A] = fa match {

      case Read(file) =>
        read(file): Id[Array[Byte]]

      case Write(file, content) =>
        write(file, content): Id[Unit]

      case Delete(file) =>
        delete(file): Id[Boolean]
    }
  }

  val futureInterpreter: DiskIO ~> Future = new (DiskIO ~> Future) {

    import UncoolIO._

    override def apply[A](fa: DiskIO[A]): Future[A] = fa match {

      case Read(file) =>
        Future { read(file) }

      case Write(file, content) =>
        Future { write(file, content) }

      case Delete(file) =>
        Future { delete(file) }
    }
  }
}

object DiskIOApp extends App {

  import dsl._
  import interpreters._

  def doCoolIO(): Free[DiskIO, String] = for {
    foo <- Free.liftM(Read("foo.txt"))
    bar <- Free.liftM(Read("bar.txt"))
    _ <- Free.liftM(Write("foobar.txt", foo ++ bar))
    foobar <- Free.liftM(Read("foobar.txt"))
  } yield new String(foobar)

  println

  {
    println("----- Executing program with idInterpreter")
    val result: Id[String] = doCoolIO().foldMap(idInterpreter)
    println(result)
  }

  {
    import cats.instances.future._

    println("----- Executing program with futureInterpreter")
    val future: Future[String] = doCoolIO().foldMap(futureInterpreter)
    val result = Await.result(future, 3.seconds)
    println(result)
  }
}
