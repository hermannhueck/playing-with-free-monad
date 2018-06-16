package runar_bjarnason.mychanges

import mycats.myfree.Free
import mycats.{Id, InjectK, ~>}

import scala.language.higherKinds

package object interact {

  // Interact Algebra
  object algebra {

    sealed trait Interact[A]
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]
  }

  object dsl {

    import algebra._

    def ask(prompt: String): Free[Interact,String] = Free.liftF(Ask(prompt))

    def tell(msg: String): Free[Interact,Unit] = Free.liftF(Tell(msg))

    // Console interpreter
    object Console extends (Interact ~> Id) {

      def apply[A](i: Interact[A]): Id[A] = i match {
        case Ask(prompt) =>
          println(prompt)
          scala.io.StdIn.readLine
        case Tell(msg) =>
          println(msg)
      }
    }

    type Tester[A] = Map[String, String] => (List[String], A)

    // TestConsole interpreter
    object TestConsole extends (Interact ~> Tester) {

      def apply[A](i: Interact[A]): Tester[A] = i match {
        case Ask(prompt) => map => (List(), map(prompt))
        case Tell(msg) => _ => (List(msg), ())
      }
    }

    class Interacts[F[_]](implicit I: InjectK[Interact,F]) {

      def ask(prompt: String): Free[F,String] = Free.inject(Ask(prompt))

      def tell(msg: String): Free[F,Unit] = Free.inject(Tell(msg))
    }

    object Interacts {
      implicit def instance[F[_]](implicit I: InjectK[Interact,F]): Interacts[F] = new Interacts[F]
    }
  }
}
