package app6.kvs

import scala.language.higherKinds
import cats.InjectK
import cats.free.Free

object dsl {

  import app6.model.Cat

  trait KVStore[A] extends Product with Serializable
  final case class Put(key: String, value: Cat) extends KVStore[Unit]
  final case class Get(key: String) extends KVStore[Option[Cat]]
  case object GetAll extends KVStore[List[Cat]]
  final case class Delete(key: String) extends KVStore[Option[Cat]]

  def put(key: String, value: Cat): Free[KVStore, Unit] = Free.liftF(Put(key: String, value: Cat))
  def get(key: String): Free[KVStore, Option[Cat]] = Free.liftF(Get(key))
  def getAll: Free[KVStore, List[Cat]] = Free.liftF(GetAll)
  def delete(key: String): Free[KVStore, Option[Cat]] = Free.liftF(Delete(key))

  class KVSOps[F[_]](implicit KV: InjectK[KVStore, F]) {
    def put(key: String, value: Cat): Free[F, Unit] = Free.inject[KVStore, F](Put(key: String, value: Cat))
    def get(key: String): Free[F, Option[Cat]] = Free.inject[KVStore, F](Get(key: String))
    def getAll: Free[F, List[Cat]] = Free.inject[KVStore, F](GetAll)
    def delete(key: String): Free[F, Option[Cat]] = Free.inject[KVStore, F](Delete(key: String))
  }

  object KVSOps {
    implicit def kvsOps[F[_]](implicit IO: InjectK[KVStore, F]): KVSOps[F] = new KVSOps[F]
  }
}
