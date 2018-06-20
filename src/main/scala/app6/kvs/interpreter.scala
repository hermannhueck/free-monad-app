package app6.kvs

import cats.free.Free
import cats.{Id, ~>}
import app6.model.Cat

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object interpreter {

  import dsl._

  def KvsInterpreter: KVStore ~> Id = new (KVStore ~> Id) {

    var kvs: Map[String, Cat] = Map.empty

    override def apply[A](fa: KVStore[A]): Id[A] = fa match {
      case Put(key, value) =>
        kvs = kvs.updated(key, value)
        () : Id[Unit]
      case Get(key) =>
        kvs.get(key) : Id[Option[Cat]]
      case GetAll =>
        kvs.values.toList : Id[List[Cat]]
      case Delete(key) =>
        val maybeCat = kvs.get(key)
        kvs = kvs - key
        maybeCat : Id[Option[Cat]]
    }
  }


  object KVSAsyncInterpreter extends (KVStore ~> Future) {

    var kvs: Map[String, Cat] = Map.empty

    override def apply[A](fa: KVStore[A]): Future[A] = fa match {
      case Put(key, value) => Future {
        kvs = kvs.updated(key, value)
      }
      case Get(key) => Future {
        kvs.get(key)
      }
      case GetAll => Future {
        kvs.values.toList
      }
      case Delete(key) => Future {
        val value = kvs.get(key)
        kvs = kvs - key
        value
      }
    }
  }


  type KVStoreFree[A] = Free[KVStore, A]

  object KVSFreeSyncInterpreter extends (KVStoreFree ~> Id) {
    override def apply[A](fa: KVStoreFree[A]): Id[A] = fa.foldMap(KvsInterpreter)
  }
}
