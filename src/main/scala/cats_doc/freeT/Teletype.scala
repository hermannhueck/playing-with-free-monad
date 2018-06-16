/*
  Original code copied from
  https://typelevel.org/cats/datatypes/freemonad.html

  --- FreeT

 */

package cats_doc.freeT

import scala.language.higherKinds

import cats._
import cats.data._
import cats.free._

/* A base ADT for the user interaction without state semantics */
sealed abstract class Teletype[A] extends Product with Serializable
final case class WriteLine(line : String) extends Teletype[Unit]
final case class ReadLine(prompt : String) extends Teletype[String]

object TeletypeApp extends App {

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
  type Log = List[String]
  type TeletypeState[A] = State[List[String], A]

  /** Teletype smart constructors */
  object TeletypeOps {
    def writeLine(line : String) : TeletypeT[TeletypeState, Unit] =
      FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
    def readLine(prompt : String) : TeletypeT[TeletypeState, String] =
      FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
    def log(s : String) : TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
  }

  // program
  val program : TeletypeT[TeletypeState, Unit] = {
    for {
      userSaid <- TeletypeOps.readLine("what's up?!")
      _ <- TeletypeOps.log(s"user said : $userSaid")
      _ <- TeletypeOps.writeLine("thanks, see you soon!")
    } yield ()
  }

  // interpreter: Teletype ~> TeletypeState
  val interpreter: Teletype ~> TeletypeState = new (Teletype ~> TeletypeState) {

    def apply[A](fa : Teletype[A]) : TeletypeState[A] = {

      fa match {
        case ReadLine(prompt) =>
          println(prompt)
          val userInput = "hanging in here" //scala.io.StdIn.readLine()
          StateT.pure[Eval, List[String], A](userInput)
        case WriteLine(line) =>
          StateT.pure[Eval, List[String], A](println(line))
      }
    }
  }

  // import TeletypeOps._

  val state = program.foldMap(interpreter)

  val initialState = Nil

  val (stored, _) = state.run(initialState).value
  // what's up?!
  // thanks, see you soon!
  // stored: List[String] = List(user said : hanging in here)
}
