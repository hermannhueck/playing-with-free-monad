package mycats.myarrow

import mycats.mydata.EitherK
import mycats.~>

import scala.language.higherKinds

trait FunctionK[F[_],G[_]] { self =>

  def apply[A](f: F[A]): G[A]

  def or[H[_]](f: H ~> G): EitherK[F, H, ?] ~> G = new (EitherK[F, H, ?] ~> G) {

      def apply[A](c: EitherK[F,H,A]): G[A] = c.run match {
        case Left(fa) => self.apply(fa)
        case Right(ha) => f(ha)
      }
    }
}
