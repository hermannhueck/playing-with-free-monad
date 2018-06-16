package mycats

import mydata.EitherK

import scala.language.higherKinds

sealed trait InjectK[F[_],G[_]] {

  def inj[A](sub: F[A]): G[A]

  def prj[A](sup: G[A]): Option[F[A]]
}

object InjectK {

  implicit def injRefl[F[_]]: InjectK[F, F] = new InjectK[F,F] {
    def inj[A](sub: F[A]): F[A] = sub
    def prj[A](sup: F[A]) = Some(sup)
  }

  implicit def injLeft[F[_],G[_]]: InjectK[F,EitherK[F, G, ?]] = new InjectK[F,EitherK[F, G, ?]] {
    def inj[A](sub: F[A]) = EitherK(Left(sub))
    def prj[A](sup: EitherK[F,G,A]): Option[F[A]] = sup.run match {
      case Left(fa) => Some(fa)
      case Right(_) => None
    }
  }

  implicit def injRight[F[_],G[_],H[_]](implicit I: InjectK[F,G]): InjectK[F, EitherK[H, G, ?]] =
    new InjectK[F,EitherK[H, G, ?]] {
      def inj[A](sub: F[A]) = EitherK(Right(I.inj(sub)))
      def prj[A](sup: EitherK[H,G,A]): Option[F[A]] = sup.run match {
        case Left(_) => None
        case Right(x) => I.prj(x)
      }
    }
}
