package app5freek.kvs

import scala.language.higherKinds
import cats.InjectK
import cats.free.Free

object dsl {

  import app5freek.model.Cat

  trait KVStore[A]
  final case class Put(key: String, value: Cat) extends KVStore[Unit]
  final case class Get(key: String) extends KVStore[Option[Cat]]
  final case class Delete(key: String) extends KVStore[Option[Cat]]
}
