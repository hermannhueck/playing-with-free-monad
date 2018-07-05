/*
  Original code copied from
  http://eed3si9n.com/herding-cats/Free-monads.html
 */

package herding_cats

import scala.language.higherKinds

object ToyApp1 extends App {

  sealed trait Toy[+A, +Next]
  object Toy {
    case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
    case class Bell[Next](next: Next) extends Toy[Nothing, Next]
    case class Done() extends Toy[Nothing, Nothing]
  }

  sealed trait CharToy[+Next]
  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]

    def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
    def bell[Next](next: Next): CharToy[Next] = CharBell(next)
    def done: CharToy[Nothing] = CharDone()
  }

  val tOutput: Toy.Output[Char, Toy.Done] = Toy.Output('A', Toy.Done())
  println(tOutput)

  val tBell = Toy.Bell(Toy.Output('A', Toy.Done()))
  println(tBell)

  import CharToy._

  val ctOutput: CharToy[CharToy[Nothing]] = output('A', done)
  println(ctOutput)

  val ctBell: CharToy[CharToy[CharToy[Nothing]]] = bell(output('A', done))
  println(ctBell)

  // but unfortunately this doesn’t work because every time I want to add a command, it changes the type.
  // Let’s define Fix: see above

  case class Fix[F[_]](f: F[Fix[F]])
  object Fix {
    def fix(toy: CharToy[Fix[CharToy]]): Fix[CharToy] = Fix[CharToy](toy)
  }

  import Fix._

  val fctOutput: Fix[CharToy] = fix(output('A', fix(done)))
  println(fctOutput)

  val fctBell: Fix[CharToy] = fix(bell(fix(output('A', fix(done)))))
  println(fctBell)
}

object ToyApp2 extends App {

  sealed trait CharToy[+Next]
  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]

    def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
    def bell[Next](next: Next): CharToy[Next] = CharBell(next)
    def done: CharToy[Nothing] = CharDone()
  }

  import cats.Functor

  sealed trait FixE[F[_], E]
  object FixE {
    case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
    case class Throwy[F[_], E](e: E) extends FixE[F, E]

    def fix[E](toy: CharToy[FixE[CharToy, E]]): FixE[CharToy, E] =
      Fix[CharToy, E](toy)
    def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)
    def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])
                                     (f: E1 => FixE[F, E2]): FixE[F, E2] = ex match {
      case Fix(x)    => Fix[F, E2](Functor[F].map(x) {catchy(_)(f)})
      case Throwy(e) => f(e)
    }
  }

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
      case o: CharToy.CharOutput[A] => CharToy.CharOutput(o.a, f(o.next))
      case b: CharToy.CharBell[A]   => CharToy.CharBell(f(b.next))
      case CharToy.CharDone()       => CharToy.CharDone()
    }
  }

  import CharToy._
  import FixE._

  case class IncompleteException()

  def subroutine: FixE[CharToy, IncompleteException] = fix[IncompleteException](
    output('A',
      throwy[CharToy, IncompleteException](IncompleteException())))

  def program: FixE[CharToy, Nothing] = catchy[CharToy, IncompleteException, Nothing](subroutine) { _ =>
    fix[Nothing](bell(fix[Nothing](done)))
  }
}

object ToyApp3 extends App {

  import cats.{Functor, Show}
  import cats.instances.unit._
  import cats.instances.char._
  import cats.free.Free

  sealed trait CharToy[+Next]
  object CharToy {
    case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
    case class CharBell[Next](next: Next) extends CharToy[Next]
    case class CharDone() extends CharToy[Nothing]

    implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
      def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
        case o: CharOutput[A] => CharOutput(o.a, f(o.next))
        case b: CharBell[A]   => CharBell(f(b.next))
        case CharDone()       => CharDone()
      }
    }
    def output(a: Char): Free[CharToy, Unit] =
      Free.liftF[CharToy, Unit](CharOutput(a, ()))
    def bell: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharBell(()))
    def done: Free[CharToy, Unit] = Free.liftF[CharToy, Unit](CharDone())
    def pure[A](a: A): Free[CharToy, A] = Free.pure[CharToy, A](a)
  }

  import CharToy._

  val subroutine: Free[CharToy, Unit] = output('A')

  val program = for {
    _ <- subroutine
    _ <- bell
    _ <- done
  } yield ()

  def showProgram[R: Show](p: Free[CharToy, R]): String =
    p.fold({ r: R => "return " + Show[R].show(r) + "\n" },
      {
        case CharOutput(a, next) =>
          "output " + Show[Char].show(a) + "\n" + showProgram(next)
        case CharBell(next) =>
          "bell " + "\n" + showProgram(next)
        case CharDone() =>
          "done\n"
      })

  println("\n--> showProgram(program)")
  println(showProgram(program))

  println("--> showProgram(output('A'))")
  println(showProgram(output('A')))

  println("--> showProgram(pure('A') flatMap output)")
  println(showProgram(pure('A') flatMap output))

  println("--> showProgram(output('A') flatMap pure)")
  println(showProgram(output('A') flatMap pure))

  println("--> showProgram((output('A') flatMap { _ => done }) flatMap { _ => output('C') })")
  println(showProgram((output('A') flatMap { _ => done }) flatMap { _ => output('C') }))

  println("--> showProgram(output('A') flatMap { _ => (done flatMap { _ => output('C') }) })")
  println(showProgram(output('A') flatMap { _ => (done flatMap { _ => output('C') }) }))
}
