package app6.log

import cats.free.Free
import cats.InjectK

import scala.language.higherKinds

object dsl {

  sealed trait Log[A] extends Product with Serializable
  final case class Info(msg: String) extends Log[Unit]
  final case class Warn(msg: String) extends Log[Unit]
  final case class Error(msg: String) extends Log[Unit]

  class LogOps[F[_]](implicit LG: InjectK[Log, F]) {
    def info(msg: String): Free[F, Unit] = Free.inject[Log, F](Info(msg))
    def warn(msg: String): Free[F, Unit] = Free.inject[Log, F](Warn(msg))
    def error(msg: String): Free[F, Unit] = Free.inject[Log, F](Error(msg))
  }

  object LogOps {
    implicit def logOps[F[_]](implicit LG: InjectK[Log, F]): LogOps[F] = new LogOps[F]
  }
}
