/*
  Original code copied from
  http://blog.higher-order.com/assets/trampolines.pdf

  Code from the article
  "Stackless Scala With Free Monads"
  by Rúnar Bjarnason at Scala Days, Berlin, 2014

 */

package runar_bjarnason.trampoline

sealed trait Trampoline[+A] extends Product with Serializable {
  final def runT: A =
    this match {
      case More(k) => k().runT
      case Done(v) => v
    }
}
case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A) extends Trampoline[A]

object EvenOddTrampolineApp extends App {

  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(true)
      case x :: xs => More(() => odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(false)
      case x :: xs => More(() => even(xs))
    }

  val res1 = even(List(1, 2, 3)).runT
  println(res1)

  try {
    val res2 = even((0 to 3000000).toList).runT // No StackOverflowError
    println(res2)
  } catch {
    case e: Throwable =>
      println(e.getMessage)
  }
}
