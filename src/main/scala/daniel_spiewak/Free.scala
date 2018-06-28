package daniel_spiewak

import cats.Monad

import scala.language.higherKinds

trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

sealed trait Free[F[_], A] {

  import Free._

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(a => pure(f(a)))

  def foldMap[G[_]: Monad](nt: F ~> G): G[A] = this match {
    case Pure(a) => Monad[G].pure(a)
    case Suspend(fa) => nt(fa)
    case Bind(target, f) =>
      Monad[G].flatMap(target.foldMap(nt)) { e =>
        f(e).foldMap(nt)
      }
  }
}

object Free {

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  case class Bind[F[_], E, A](target: Free[F, E], f: E => Free[F, A]) extends Free[F, A]

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def liftM[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)
}
