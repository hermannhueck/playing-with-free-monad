package mycats.myfree

import mycats.{InjectK, Monad, ~>}

import scala.language.higherKinds

sealed trait Free[F[_], A] {

  import Free.{FlatMap, Pure}

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Pure(a) => f(a)
      case FlatMap(fx, g) =>
        FlatMap(fx, g andThen (_ flatMap f))
    }

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Pure(f(a)))

  def foldMap[G[_] : Monad](f: F ~> G): G[A] =
    this match {
      case Pure(a) => Monad[G].pure(a)
      case FlatMap(fx, g) =>
        Monad[G].flatMap(f(fx)) { a =>
          g(a).foldMap(f)
        }
    }
}


object Free {

  private[myfree] final case class Pure[F[_], A](a: A) extends Free[F, A]

  private[myfree] final case class FlatMap[F[_], I, A](a: F[I], f: I => Free[F, A]) extends Free[F, A]


  def pure[S[_], A](a: A): Free[S, A] = Pure[S, A](a)

  def liftF[F[_], A](fa: F[A]): Free[F, A] =
    FlatMap(fa, Pure(_: A))

  def inject[F[_], G[_], A](fa: F[A])(implicit I: InjectK[F, G]): Free[G, A] =
    FlatMap(I.inj(fa), Pure(_: A))
}
