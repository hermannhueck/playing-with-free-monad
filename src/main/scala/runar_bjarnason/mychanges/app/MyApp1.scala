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
import cats.free.Free
import cats.Id
import cats.Monad
*/

import runar_bjarnason.mychanges._
import mycats.{Id, Monad}
import mycats.myfree.Free

import interact.algebra._
import interact.dsl._

object MyApp1 extends App {

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

  def consoleProg: Free[Interact, String] = {
    for {
      first <- ask("What's your first name?")
      second <- ask("What's your second name?")
      _ <- tell(s"Hello $first $second!")
    } yield s"$first.$second"
  }

  def runConsole: Id[String] = consoleProg.foldMap(Console)
  //val consoleResult = runConsole
  //println(s"\nconsoleResult = $consoleResult")

  val consoleTestMap = Map(
    "What's your first name?" -> "john",
    "What's your second name?" -> "snow"
  )

  def runTestConsole = consoleProg.foldMap(TestConsole)
  val testConsoleResult: (List[String], String) = runTestConsole(consoleTestMap)
  println(s"\ntestConsoleResult = $testConsoleResult")
  println

  assert(testConsoleResult._2 == "john.snow")
}
