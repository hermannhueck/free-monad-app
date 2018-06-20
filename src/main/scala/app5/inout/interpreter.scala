package app5.inout

import cats.{Id, ~>}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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
