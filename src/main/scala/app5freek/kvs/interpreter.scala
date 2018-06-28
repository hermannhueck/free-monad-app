package app5freek.kvs

import cats.{Id, ~>}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object interpreter {

  import app5freek.model.Cat
  import dsl._

  object KVSInterpreter extends (KVStore ~> Id) {

    var kvs: Map[String, Cat] = Map.empty

    override def apply[A](fa: KVStore[A]): Id[A] = fa match {
      case Put(key, value) =>
        kvs = kvs.updated(key, value)
        () : Id[Unit]
      case Get(key) =>
        kvs.get(key) : Id[Option[Cat]]
      case Delete(key) =>
        val value = kvs.get(key)
        kvs = kvs - key
        value : Id[Option[Cat]]
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
      case Delete(key) => Future {
        val value = kvs.get(key)
        kvs = kvs - key
        value
      }
    }
  }
}
