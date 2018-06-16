import scala.language.higherKinds

package object mycats {

  type Id[A] = A

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): A = a
    def flatMap[A,B](a: A)(f: A => B) = f(a)
  }

  type ~>[F[_], G[_]] = myarrow.FunctionK[F, G]
}
