package app5freek.inout

import scala.language.higherKinds

import cats.free.Free
import freek._

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