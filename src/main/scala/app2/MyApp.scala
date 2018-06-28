package app2

import cats.free.Free
import cats.{Id, ~>}

object MyApp extends App {

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
    } yield input

    def ask2(prompt: String): Free[Inout, String] =
      printline(prompt).flatMap(_ => getline)
  }

  object interpreter {

    import dsl._

    // interpreter
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
  }

  import dsl._
  import interpreter._

  // program definition (does nothing)
  def prog: Free[Inout, (String, Int)] = for {
    name <- ask("What's your name?")
    age <- ask("What's your age?")
    _ <- printline(s"Hello $name! Your age is $age!")
  } yield (name, age.toInt)

  // program execution: invoke program with an interpreter
  println("\n----- Execute program with ConsoleInterpreter")
  val result: Id[(String, Int)] = prog.foldMap(ConsoleInterpreter)
  println(s"\nresult = $result")

  println("\n-----\n")
}
