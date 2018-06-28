package app5.inout

import scala.language.higherKinds

import cats.InjectK
import cats.free.Free

object dsl {

  // Algebra as an ADT
  trait Inout[A]
  final case class Printline(out: String) extends Inout[Unit]
  final case object Getline extends Inout[String]

  // DSL
  class IoOps[F[_]](implicit IO: InjectK[Inout, F]) {
    def printline(out: String): Free[F, Unit] = Free.inject[Inout, F](Printline(out))
    def getline: Free[F, String] = Free.inject[Inout, F](Getline)
    def ask(prompt: String): Free[F, String] = for {
      _ <- printline(prompt)
      input <- getline
    } yield input
  }

  object IoOps {
    implicit def ioOpss[F[_]](implicit IO: InjectK[Inout, F]): IoOps[F] = new IoOps[F]
  }
}