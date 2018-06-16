package mycats.mydata

import scala.language.higherKinds

case class EitherK[F[_], G[_], A](run: Either[F[A], G[A]])
