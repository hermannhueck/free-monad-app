package myfreeimpl

import cats.Id

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MyApp3 extends App {

  object dsl {

    // Algebra as an ADT
    trait Inout[A]
    final case class Printline(out: String) extends Inout[Unit]
    final case object Getline extends Inout[String]

    // DSL
    def printline(out: String): Free[Inout, Unit] = Free.liftF(Printline(out))
    def getline: Free[Inout, String] = Free.liftF(Getline)
    def ask(prompt: String): Free[Inout, String] = for {
      _ <- printline(prompt)
      input <- getline
    } yield input // same as:
    def ask2(prompt: String): Free[Inout, String] =
      printline(prompt).flatMap(_ => getline)
  }

  object interpreter {

    import dsl._

    object ConsoleInterpreter extends (Inout ~> Id) {

      override def apply[A](fa: Inout[A]): Id[A] = fa match {
        case Printline(out) =>
          println(out)
          () : Id[Unit]
        case Getline =>
          val in = scala.io.StdIn.readLine()
          in : Id[String]
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
          () : Id[Unit]
        case Getline =>
          val in = inputs.head
          inputs -= inputs.head
          in : Id[String]
      }
    }
  }

  import dsl._
  import interpreter._

  // program definition (does nothing)

  def prog: Free[Inout, (String, Int)] = for {
    name <- ask("What's your name?")
    age <- ask("What's your age?")
    _ <- printline(s"Hello $name! Your age is $age!")
  } yield (name, age.toInt)

  def execSync(): Unit = {
    println("\n----- Execute program with ConsoleInterpreter")
    val result: Id[(String, Int)] = prog.foldMap(ConsoleInterpreter)
    println(s"result = $result")
  }
  // execSync()

  def execAsync(): Unit = {
    println("\n----- Execute program with AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val future: Future[(String, Int)] = prog.foldMap(AsyncInterpreter)
    val result = Await.result(future, 15.second)
    println(s"result = $result")
  }
  // execAsync()

  def execTest(): Unit = {
    println("\n----- Execute program with TestInterpreter")
    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val result: Id[(String, Int)] = prog.foldMap(new TestInterpreter(inputs, outputs))
    println(s"result = $result")
    println(s"outputs = $outputs")
    // Test:
    assert(result == ("John Doe", 33))
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello John Doe! Your age is 33!"))
    println("asserted outputs ok")
  }
  execTest()

  println("\n-----\n")
}
