package mycats

import scala.language.higherKinds

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A,B](a: M[A])(f: A => M[B]): M[B]
}

object Monad {
  def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
}
