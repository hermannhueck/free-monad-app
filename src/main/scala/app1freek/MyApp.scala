package app1freek

import cats.free.Free
import cats.{Id, ~>}
import freek._

object MyApp extends App {

  // Algebra as an ADT
  trait Inout[A]
  final case class Printline(out: String) extends Inout[Unit]
  case object Getline extends Inout[String]

  // DSL: no lifting or injecting needed with Freek

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

  type AppDSL = Inout :|: NilDSL
  val appDSL = DSL.Make[AppDSL]

  // val interpreter: Interpreter[PRG.Cop, Id] = ConsoleInterpreter

  // program definition (does nothing)
  def prog: Free[appDSL.Cop, (String, Int)] = for {
    _ <- Printline("What's your name?").freek[AppDSL]
    name <- Getline.freek[AppDSL]
    _ <- Printline("What's your age?").freek[AppDSL]
    age <- Getline.freek[AppDSL]
    _ <- Printline(s"Hello $name! Your age is $age!").freek[AppDSL]
  } yield (name, age.toInt)

  // program execution: the program must be combined with an interpreter

  println("\n----- Execute program with ConsoleInterpreter")
  val result: Id[(String, Int)] = prog.interpret(ConsoleInterpreter) // same as: prog.foldMap(ConsoleInterpreter.nat)
  println(s"\nresult = $result")

  println("\n-----\n")
}
