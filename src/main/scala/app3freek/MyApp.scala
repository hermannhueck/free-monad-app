package app3freek

import cats.free.Free
import cats.{Id, ~>}
import freek._

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  object dsl {

    // Algebra as an ADT
    trait Inout[A]
    final case class Printline(out: String) extends Inout[Unit]
    final case object Getline extends Inout[String]

    // DSL: no lifting or injecting needed with Freek

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

  type AppDSL = Inout :|: NilDSL
  val appDSL = DSL.Make[AppDSL]

  def prog: Free[appDSL.Cop, (String, Int)] = for {
    name <- ask("What's your name?").expand[AppDSL]
    age <- ask("What's your age?").expand[AppDSL]
    _ <- Printline(s"Hello $name! Your age is $age!").freek[AppDSL]
  } yield (name, age.toInt)

  // program execution: the program must be combined with an interpreter

  def execSync(): Unit = {
    println("\n----- Execute program with ConsoleInterpreter")
    val result: Id[(String, Int)] = prog.interpret(ConsoleInterpreter)
    println(s"result = $result")
  }

  def execAsync(): Unit = {
    println("\n----- Execute program with AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val future: Future[(String, Int)] = prog.interpret(AsyncInterpreter)
    val result = Await.result(future, 15.second)
    println(s"result = $result")
  }

  def execTest(): Unit = {
    println("\n----- Execute program with TestInterpreter")
    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val result: Id[(String, Int)] = prog.interpret(new TestInterpreter(inputs, outputs))
    println(s"result = $result")
    println(s"outputs = $outputs")
    // Test:
    assert(result == ("John Doe", 33))
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello John Doe! Your age is 33!"))
    println("asserted outputs ok")
  }

  // execSync()
  // execAsync()
  execTest()

  println("\n-----\n")
}
