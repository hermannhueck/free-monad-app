package app6.inout

import scala.language.higherKinds

import cats.InjectK
import cats.free.Free

object dsl {

  // Algebra as an ADT
  trait Inout[A] extends Product with Serializable
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