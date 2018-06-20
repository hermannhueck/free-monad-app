package app4

import cats.data.EitherK

import scala.language.higherKinds
import cats.free.Free
import cats.{Id, InjectK, ~>}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  object inout {

    object dsl {

      // Algebra as an ADT
      trait Inout[A]

      final case class Printline(out: String) extends Inout[Unit]

      final case object Getline extends Inout[String]

      // DSL
      class Inouts[F[_]](implicit IO: InjectK[Inout, F]) {
        def printline(out: String): Free[F, Unit] = Free.inject[Inout, F](Printline(out))

        def getline: Free[F, String] = Free.inject[Inout, F](Getline)

        def ask(prompt: String): Free[F, String] = for {
          _ <- printline(prompt)
          input <- getline
        } yield input
      }

      object Inouts {
        implicit def inouts[F[_]](implicit IO: InjectK[Inout, F]): Inouts[F] = new Inouts[F]
      }

    }

    object interpreter {

      import dsl._

      object ConsoleInterpreter extends (Inout ~> Id) {

        override def apply[A](fa: Inout[A]): Id[A] = fa match {
          case Printline(out) =>
            println(out)
            (): Id[Unit]
          case Getline =>
            val in = scala.io.StdIn.readLine()
            in: Id[String]
        }
      }

      object AsyncInterpreter extends (Inout ~> Future) {

        override def apply[A](fa: Inout[A]): Future[A] = fa match {
          case Printline(out) => Future {
            println(out)
          }
          case Getline => Future {
            scala.io.StdIn.readLine()
          }
        }
      }

      class TestInterpreter(inputs: ListBuffer[String], outputs: ListBuffer[String]) extends (Inout ~> Id) {

        override def apply[A](fa: Inout[A]): Id[A] = fa match {
          case Printline(out) =>
            outputs append out
            (): Id[Unit]
          case Getline =>
            val in = inputs.head
            inputs -= inputs.head
            in: Id[String]
        }
      }
    }
  }

  object kvs {

    object dsl {

      trait KVStore[A]
      final case class Put(key: String, value: Any) extends KVStore[Unit]
      final case class Get(key: String) extends KVStore[Option[Any]]
      final case class Delete(key: String) extends KVStore[Option[Any]]

      class KVSOps[F[_]](implicit KV: InjectK[KVStore, F]) {
        def put(key: String, value: Any): Free[F, Unit] = Free.inject[KVStore, F](Put(key: String, value: Any))
        def get(key: String): Free[F, Option[Any]] = Free.inject[KVStore, F](Get(key: String))
        def delete(key: String): Free[F, Option[Any]] = Free.inject[KVStore, F](Delete(key: String))
      }

      object KVSOps {
        implicit def kvsOps[F[_]](implicit IO: InjectK[KVStore, F]): KVSOps[F] = new KVSOps[F]
      }

    }

    object interpreter {

      import dsl._

      object KVSSyncInterpreter extends (dsl.KVStore ~> Id) {

        var kvs: Map[String, Any] = Map.empty

        override def apply[A](fa: dsl.KVStore[A]): Id[A] = fa match {
          case Put(key, value) =>
            kvs = kvs.updated(key, value)
            (): Id[Unit]
          case Get(key) =>
            kvs.get(key): Id[Option[Any]]
          case Delete(key) =>
            val value = kvs.get(key)
            kvs = kvs - key
            value: Id[Option[Any]]
        }
      }

      object KVSAsyncInterpreter extends (dsl.KVStore ~> Future) {

        var kvs: Map[String, Any] = Map.empty

        override def apply[A](fa: dsl.KVStore[A]): Future[A] = fa match {
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
  }

  import inout.dsl._
  import inout.interpreter._
  import kvs.dsl._
  import kvs.interpreter._

  type AppDSL[A] = EitherK[KVStore, Inout, A]

  // program definition (does nothing)

  def prog(implicit io: Inouts[AppDSL], kvs: KVSOps[AppDSL]): Free[AppDSL, (String, Option[Int])] = {
    for {
      name <- io.ask("What's your name?")
      age <- io.ask("What's your age?")
      _ <- kvs.put(name, age.toInt)
      _ <- io.printline(s"Hello $name! Your age is $age!")
      optAge <- kvs.get(name)
    } yield (name, optAge.asInstanceOf[Option[Int]])
  }

  // program execution: the program must be combined with an interpreter

  def execSync(): Unit = {
    println("\n----- Execute program with KVSSyncInterpreter and ConsoleInterpreter")
    val result1: Id[(String, Option[Int])] = prog.foldMap(KVSSyncInterpreter or ConsoleInterpreter)
    println(s"result1 = $result1")
  }

  def execAsync(): Unit = {
    println("\n----- Execute program with KVSAsyncInterpreter and AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val future: Future[(String, Option[Int])] = prog.foldMap(KVSAsyncInterpreter or AsyncInterpreter)
    val result2 = Await.result(future, 15.second)
    println(s"result2 = $result2")
  }

  def execTest(): Unit = {
    println("\n----- Execute program with KVSSyncInterpreter and TestInterpreter")
    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val result3: Id[(String, Option[Int])] = prog.foldMap(KVSSyncInterpreter or new TestInterpreter(inputs, outputs))
    println(s"result3 = $result3")
    println(s"outputs = $outputs")
    // Test:
    assert(result3 == ("John Doe", Some(33)))
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello John Doe! Your age is 33!"))
    println("asserted outputs ok")
  }

  // execSync()
  // execAsync()
  execTest()

  println("\n-----\n")
}
