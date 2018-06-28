package app2freek

import cats.free.Free
import cats.{Id, ~>}
import freek._

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

  type AppDSL = Inout :|: NilDSL
  val appDSL = DSL.Make[AppDSL]

  def prog: Free[appDSL.Cop, (String, Int)] = for {
    name <- ask("What's your name?").expand[AppDSL] // ask must be expanded to Printline/Getline
    age <- ask("What's your age?").expand[AppDSL]
    _ <- Printline(s"Hello $name! Your age is $age!").freek[AppDSL]
  } yield (name, age.toInt)

  // program execution: the program must be combined with an interpreter

  println("\n----- Execute program with ConsoleInterpreter")
  val result: Id[(String, Int)] = prog.interpret(ConsoleInterpreter)
  println(s"\nresult = $result")

  println("\n-----\n")
}
