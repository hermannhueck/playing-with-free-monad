/*
  Original code copied from
  http://blog.higher-order.com/assets/trampolines.pdf

  Code from the article
  "Stackless Scala With Free Monads"
  by Rúnar Bjarnason at Scala Days, Berlin, 2014

 */

package runar_bjarnason.trampoline

object EvenOddApp extends App {

  def even[A](ns: List[A]): Boolean =
    ns match {
      case Nil => true
      case x :: xs => odd(xs)
    }

  def odd[A](ns: List[A]): Boolean =
    ns match {
      case Nil => false
      case x :: xs => even(xs)
    }

  val res1 = even(List(1, 2, 3))
  println(res1)

  try {
    val res2 = even((0 to 3000000).toList) // StackOverflowError
    println(res2)
  } catch {
    case e: Throwable =>
      println(e.getMessage)
  }
}
