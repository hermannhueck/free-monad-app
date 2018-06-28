package app4freek

import scala.language.higherKinds

import cats.free.Free
import cats.{Id, InjectK, ~>}
import freek._

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

      type PRG = Inout :|: NilDSL
      val prg = DSL.Make[PRG]

      def ask(prompt: String): Free[prg.Cop, String] = for {
        _ <- Printline(prompt).freek[PRG]
        input <- Getline.freek[PRG]
      } yield input

      def ask2(prompt: String): Free[prg.Cop, String] =
        Printline(prompt).freek[PRG].flatMap(_ => Getline.freek[PRG])
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
      final case class Put(key: String, value: Int) extends KVStore[Unit]
      final case class Get(key: String) extends KVStore[Option[Int]]
      final case class Delete(key: String) extends KVStore[Option[Int]]
    }

    object interpreter {

      import dsl._

      object KVSSyncInterpreter extends (dsl.KVStore ~> Id) {

        var kvs: Map[String, Int] = Map.empty

        override def apply[A](fa: dsl.KVStore[A]): Id[A] = fa match {
          case Put(key, value) =>
            kvs = kvs.updated(key, value)
            (): Id[Unit]
          case Get(key) =>
            kvs.get(key): Id[Option[Int]]
          case Delete(key) =>
            val value = kvs.get(key)
            kvs = kvs - key
            value: Id[Option[Int]]
        }
      }

      object KVSAsyncInterpreter extends (dsl.KVStore ~> Future) {

        var kvs: Map[String, Int] = Map.empty

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

  // program definition (does nothing)

  type AppDSL = Inout :|: KVStore :|: NilDSL
  val appDSL = DSL.Make[AppDSL]

  def prog: Free[appDSL.Cop, (String, Option[Int])] =
    for {
      name <- ask("What's your name?").expand[AppDSL]
      age <- ask("What's your age?").expand[AppDSL]
      _ <- Put(name, age.toInt).freek[AppDSL]
      _ <- Printline(s"Hello $name! Your age is $age!").freek[AppDSL]
      optAge <- Get(name).freek[AppDSL]
    } yield (name, optAge)

  // program execution: the program must be combined with an interpreter

  def execSync(): Unit = {
    println("\n----- Execute program with KVSSyncInterpreter and ConsoleInterpreter")
    val result: Id[(String, Option[Int])] = prog.interpret(ConsoleInterpreter :&: KVSSyncInterpreter)
    println(s"result = $result")
  }

  def execAsync(): Unit = {
    println("\n----- Execute program with KVSAsyncInterpreter and AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val future: Future[(String, Option[Int])] = prog.interpret(AsyncInterpreter :&: KVSAsyncInterpreter)
    val result = Await.result(future, 15.second)
    println(s"result = $result")
  }

  def execTest(): Unit = {
    println("\n----- Execute program with KVSSyncInterpreter and TestInterpreter")
    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val result: Id[(String, Option[Int])] = prog.interpret(new TestInterpreter(inputs, outputs) :&: KVSSyncInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
    // Test:
    assert(result == ("John Doe", Some(33)))
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello John Doe! Your age is 33!"))
    println("asserted outputs ok")
  }

  // execSync()
  // execAsync()
  execTest()

  println("\n-----\n")
}
