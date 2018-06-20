package app5.kvs

import scala.language.higherKinds
import cats.InjectK
import cats.free.Free

object dsl {

  import app5.model.Cat

  trait KVStore[A]
  final case class Put(key: String, value: Cat) extends KVStore[Unit]
  final case class Get(key: String) extends KVStore[Option[Cat]]
  final case class Delete(key: String) extends KVStore[Option[Cat]]

  class KVSOps[F[_]](implicit KV: InjectK[KVStore, F]) {
    def put(key: String, value: Cat): Free[F, Unit] = Free.inject[KVStore, F](Put(key: String, value: Cat))
    def get(key: String): Free[F, Option[Cat]] = Free.inject[KVStore, F](Get(key: String))
    def delete(key: String): Free[F, Option[Cat]] = Free.inject[KVStore, F](Delete(key: String))
  }

  object KVSOps {
    implicit def kvsOps[F[_]](implicit IO: InjectK[KVStore, F]): KVSOps[F] = new KVSOps[F]
  }
}
