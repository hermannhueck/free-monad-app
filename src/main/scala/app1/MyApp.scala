package app1

import cats.free.Free
import cats.{Id, ~>}

object MyApp extends App {

  // Algebra as an ADT
  trait Inout[A]
  final case class Printline(out: String) extends Inout[Unit]
  case object Getline extends Inout[String]

  // DSL
  def printline(out: String): Free[Inout, Unit] = Free.liftF(Printline(out))
  def getline: Free[Inout, String] = Free.liftF(Getline)

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

  // program definition (does nothing)
  def prog: Free[Inout, (String, Int)] = for {
    _ <- printline("What's your name?")
    name <- getline
    _ <- printline("What's your age?")
    age <- getline
    _ <- printline(s"Hello $name! Your age is $age!")
  } yield (name, age.toInt)

  // program execution: the program must be combined with an interpreter

  println("\n----- Execute program with ConsoleInterpreter")
  val result: Id[(String, Int)] = prog.foldMap(ConsoleInterpreter)
  println(s"\nresult = $result")

  println("\n-----\n")
}
