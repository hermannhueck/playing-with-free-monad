/*
  Original code copied from
  https://typelevel.org/cats/datatypes/freemonad.html

  --- Using Free Monads

 */

package cats_doc.kvstore

import cats.Id

import scala.language.higherKinds
import cats.free.Free

object adt { // ADT

  sealed trait KVStoreA[A]

  case class Put[T](key: String, value: T) extends KVStoreA[Unit]

  case class Get[T](key: String) extends KVStoreA[Option[T]]

  case class Delete(key: String) extends KVStoreA[Unit]

}

object dsl { // --- the DSL

  import adt._

  type KVStore[A] = Free[KVStoreA, A]

  // Put returns nothing (i.e. Unit).
  def put[T](key: String, value: T): KVStore[Unit] =
    Free.liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: String): KVStore[Option[T]] =
    Free.liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    Free.liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

object programs { // --- the program written in that DSL

  import dsl._

  // the prog1 operates on the KVS and finally returns the number of wild-cats
  def prog: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      numWildCats <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield numWildCats
}

object KVStoreApp extends App {

  println("\n-----")

  def runImpure: Id[Option[Int]] = {

    import adt._
    import programs.prog
    import cats.{Id, ~>}
    import scala.collection.mutable

    // --- an impure interpreter

    // the program will crash if a key is not found,
    // or if a type is incorrectly specified.
    val impureCompiler: adt.KVStoreA ~> Id = new (adt.KVStoreA ~> Id) {

      // a very simple (and imprecise) key-value store
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

    // run the compiled interpreter

    prog.foldMap(impureCompiler)
  }

  val result: Option[Int] = runImpure
  println("--> " + result)

  def runPure: (Map[String, Any], Option[Int]) = {

    import adt._
    import programs._
    import cats.data.State
    import cats.~>

    // another (pure interpreter) using the state monad

    type KVStoreState[A] = State[Map[String, Any], A]

    val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {

      def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
        fa match {
          case Put(key, value) => State.modify(_.updated(key, value))
          case Get(key) => State.inspect(_.get(key).map(_.asInstanceOf[A]))
          case Delete(key) => State.modify(_ - key)
        }
    }

    // run with the pure interpreter

    prog.foldMap(pureCompiler).run(Map.empty).value
  }

  val result2: (Map[String, Any], Option[Int]) = runPure
  println("\n" + result2)

  println("\n-----\n")
}