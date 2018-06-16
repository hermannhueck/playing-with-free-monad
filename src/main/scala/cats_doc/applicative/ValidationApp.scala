/*
  Original code copied from
  https://typelevel.org/cats/datatypes/freeapplicative.html
 */

package cats_doc.applicative

import cats.arrow.FunctionK
import cats.~>
import cats.free.FreeApplicative
import cats.syntax.apply._ // for mapN

// ADT
sealed abstract class ValidationOp[A]
case class Size(size: Int) extends ValidationOp[Boolean]
case object HasNumber extends ValidationOp[Boolean]

object ValidationApp extends App {

  // DSL
  type Validation[A] = FreeApplicative[ValidationOp, A]

  def size(size: Int): Validation[Boolean] = FreeApplicative.lift(Size(size))

  val hasNumber: Validation[Boolean] = FreeApplicative.lift(HasNumber)

  // (applicative) program
  val prog: Validation[Boolean] = (size(5), hasNumber).mapN {
    case (fst, snd) => fst && snd
  }

  // ----- Sequential Validation
  // ---------------------------

  // a function that takes a string as input
  type FromString[A] = String => A

  val compiler: ValidationOp ~> FromString = new (ValidationOp ~> FromString) { // new FunctionK[ValidationOp, FromString] {

    def apply[A](fa: ValidationOp[A]): FromString[A] =

      str => fa match {
        case Size(size) => str.size >= size
        case HasNumber => str.exists(c => "0123456789".contains(c))
      }
  }


  import cats.instances.function._ // for type FromString[A] = String => A

  val sequentialValidator = prog.foldMap[FromString](compiler)

  println("\n----- Sequential Validation:")
  println(sequentialValidator("1234")) // --> false
  println(sequentialValidator("12345")) // --> true
  println(sequentialValidator("123456")) // --> true
  println(sequentialValidator("1234X")) // --> true
  println(sequentialValidator("1XXXX")) // --> true
  println(sequentialValidator("XXXXX")) // --> false


  // ----- Parallelism
  // --------------------

  import cats.data.Kleisli
  import scala.concurrent.{Future, Await}
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  // recall Kleisli[Future, String, A] is the same as String => Future[A]
  type ParValidator[A] = Kleisli[Future, String, A]

  val parCompiler: ValidationOp ~> ParValidator = new (ValidationOp ~> ParValidator) { // new FunctionK[ValidationOp, ParValidator] {

    def apply[A](fa: ValidationOp[A]): ParValidator[A] =
      Kleisli { str => fa match {
        case Size(size) => Future { str.size >= size }
        case HasNumber => Future { str.exists(c => "0123456789".contains(c)) }
      }
    }
  }

  import cats.instances.future._  // for type ParValidator[A] = Kleisli[Future, String, A]

  val parallelValidator = prog.foldMap[ParValidator](parCompiler)

  println("\n----- Parallel Validation:")
  println(Await.result(parallelValidator("1234"), 1.second)) // false
  println(Await.result(parallelValidator("12345"), 1.second)) // true
  println(Await.result(parallelValidator("123456"), 1.second)) // true
  println(Await.result(parallelValidator("1234X"), 1.second)) // true
  println(Await.result(parallelValidator("1XXXX"), 1.second)) // true
  println(Await.result(parallelValidator("XXXXX"), 1.second)) // false


  // ----- Logging
  // --------------------

  import cats.data.Const

  type Log[A] = Const[List[String], A]

  val logCompiler: ~>[ValidationOp, Log] = new FunctionK[ValidationOp, Log] {

    def apply[A](fa: ValidationOp[A]): Log[A] = fa match {
      case Size(size) => Const(List(s"size >= $size"))
      case HasNumber => Const(List("has number"))
    }
  }

  import cats.instances.list._  // for type Log[A] = Const[List[String], A]

  def logValidation[A](validation: Validation[A]): List[String] = validation.foldMap[Log](logCompiler).getConst

  println("\n----- Log Validation:")

  val log1: List[String] = logValidation(prog)
  // log1: List[String] = List(size >= 5, has number)
  println(log1)

  val log2: List[String] = logValidation(size(5) *> hasNumber *> size(10))
  // log2: List[String] = List(size >= 5, has number, size >= 10)
  println(log2)

  val log3: List[String] = logValidation((hasNumber, size(3)).mapN(_ || _))
  // log3: List[String] = List(has number, size >= 3)
  println(log3)


  // ----- Why not both?
  // --------------------

  import cats.data.Tuple2K

  type ValidateAndLog[A] = Tuple2K[ParValidator, Log, A]

  val prodCompiler: FunctionK[ValidationOp, ValidateAndLog] = parCompiler and logCompiler

  val prodValidation: ValidateAndLog[Boolean] = prog.foldMap[ValidateAndLog](prodCompiler)

  println("\n-----\n")
}
