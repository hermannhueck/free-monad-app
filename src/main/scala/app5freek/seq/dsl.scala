package app5freek.seq

import scala.language.higherKinds

import cats.InjectK
import cats.free.Free

object dsl {

  sealed trait Sequence[A]
  case object NextId extends Sequence[Long]
}
