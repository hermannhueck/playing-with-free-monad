/*
  Original code copied from
  https://typelevel.org/cats/datatypes/freemonad.html

  --- Composing Free monads ADTs.

 */

package cats_doc.compose

import scala.language.higherKinds

import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}

/* Handles user interaction */
sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

/* Represents persistence operations */
sealed trait DataOp[A]
case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
  def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
  def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
}

object Interacts {
  implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
}

class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
  def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
  def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
}

object DataSource {
  implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
}

object ComposingADTs extends App {

  println("\n-----")

  type CatsApp[A] = EitherK[DataOp, Interact, A]

  // the program

  def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {

    import I._, D._

    for {
      cat <- ask("What's the kitty's name?")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
    } yield ()
  }

  // 2 composed interpreters

  object ConsoleCatsInterpreter extends (Interact ~> Id) {

    def apply[A](i: Interact[A]): Id[A] = i match {

      case Ask(prompt) =>
        println(prompt)
        val reply: String = scala.io.StdIn.readLine()
        reply
      case Tell(msg) =>
        println(msg)
        ()
    }
  }

  object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

    private[this] val memDataSet = new scala.collection.mutable.ListBuffer[String]

    def apply[A](fa: DataOp[A]): Id[A] = fa match {

      case AddCat(a) =>
        memDataSet.append(a)
        ()
      case GetAllCats() =>
        memDataSet.toList
    }
  }

  val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter

  import DataSource._, Interacts._

  val evaled: Unit = program.foldMap(interpreter)
  // What's the kitty's name?
  // List(snuggles)
  // evaled: Unit = ()

  println("\n-----\n")
}