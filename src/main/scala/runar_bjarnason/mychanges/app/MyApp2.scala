/*
  Original code copied from
  https://gist.github.com/runarorama/a8fab38e473fafa0921d

  Code from the talk
  "Composable application architecture with reasonably priced monads"
  by Rúnar Bjarnason at Scala Days, Berlin, 2014

 */

package runar_bjarnason.mychanges.app

import scala.language.higherKinds

/*
import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}
import cats.Monad
*/

import runar_bjarnason.mychanges._
import mycats.{Id, Monad, ~>}
import mycats.mydata.EitherK
import mycats.myfree.Free

import auth.algebra._
import auth.dsl._
import interact.algebra._
import interact.dsl._

object MyApp2 extends App {

  implicit val testerMonad: Monad[Tester] = new Monad[Tester] {
    def pure[A](a: A): Map[String, String] => (List[Nothing], A) = _ => (List(), a)
    def flatMap[A,B](t: Tester[A])(f: A => Tester[B]): Map[String, String] => (List[String], B) =
      m => {
        val (o1, a) = t(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }

    // override def tailRecM[A, B](a: A)(f: A => Tester[Either[A, B]]): Tester[B] = ???
  }

  val KnowSecret = "KnowSecret"

  def progInteractWithAuth[F[_]](implicit I: Interacts[F], A: Auths[F]): Free[F, Boolean] = {
    import A._
    import I._
    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      maybeUser <- login(uid, pwd)
      permitted <- maybeUser.map(hasPermission(_, KnowSecret)).getOrElse(Free.pure(false))
      _ <- if (permitted) tell("UUDDLRLRBA") else tell("Go away!")
    } yield permitted
  }

  type Coproduct[F[_], G[_], A] = EitherK[F, G, A] // Coproduct has become EitherK
  type App[A] = Coproduct[Auth, Interact, A]

  val app: Free[App, Boolean] = progInteractWithAuth[App]

  val Authentication: Auth ~> Id = new (Auth ~> Id) {
    def apply[A](a: Auth[A]): Id[A] = a match {
      case Login(uid, pwd) =>
        if (uid == "john.snow" && pwd == "Ghost")
          Some(User("john.snow"))
        else None
      case HasPermission(u, _) =>
        u.id == "john.snow"
    }
  }

  val TestAuth: Auth ~> Tester = new (Auth ~> Tester) {
    def apply[A](a: Auth[A]): Tester[A] = a match {
      case Login(uid, pwd) =>
        if (uid == "john.snow" && pwd == "Ghost")
          map => (List(uid, pwd), Some(User("john.snow")))
        else
          map => (List(uid), None)
      case HasPermission(u, _) =>
        map => (List(), map(u.id) == "Ghost")
    }
  }

  def runApp: Id[Boolean] = app.foldMap(Authentication or Console)

  def runTest: Tester[Boolean] = app.foldMap(TestAuth or TestConsole)

  //println("\npermitted = " + runApp)

  val testMap = Map(
    "What's your user ID?" -> "john.snow",
    "Password, please." -> "Ghost",
    "john.snow" -> "Ghost"
  )

  val result: (List[String], Boolean) = runTest(testMap)
  val permitted = result._2

  println
  println(result)
  println
  assert(permitted)

  val testMap2 = Map(
    "What's your user ID?" -> "jack.ripper",
    "Password, please." -> "Knife",
    "jack.ripper" -> "Knife"
  )

  val result2: (List[String], Boolean) = runTest(testMap2)
  val permitted2 = result2._2

  println(result2)
  println
  assert(!permitted2)
}
