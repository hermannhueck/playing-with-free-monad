package runar_bjarnason.mychanges

import mycats.InjectK
import mycats.myfree.Free

import scala.language.higherKinds

package object auth {

  object algebra {

    type UserID = String
    type Password = String
    type Permission = String

    case class User(id: UserID)

    // Auth Algebra
    sealed trait Auth[A]
    case class Login(u: UserID, p: Password) extends Auth[Option[User]]
    case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  }

  object dsl {

    import algebra._

    class Auths[F[_]](implicit I: InjectK[Auth, F]) {

      def login(id: UserID, pwd: Password): Free[F, Option[User]] = Free.inject(Login(id, pwd))

      def hasPermission(u: User, p: Permission): Free[F, Boolean] = Free.inject(HasPermission(u, p))
    }

    object Auths {
      implicit def instance[F[_]](implicit I: InjectK[Auth, F]): Auths[F] = new Auths[F]
    }
  }
}
