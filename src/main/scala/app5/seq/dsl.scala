package app5.seq

import scala.language.higherKinds

import cats.InjectK
import cats.free.Free

object dsl {

  sealed trait Sequence[A]
  case object NextId extends Sequence[Long]

  class SeqOps[F[_]](implicit KV: InjectK[Sequence, F]) {
    def nextId: Free[F, Long] = Free.inject[Sequence, F](NextId)
  }

  object SeqOps {
    implicit def seqOps[F[_]](implicit IO: InjectK[Sequence, F]): SeqOps[F] = new SeqOps[F]
  }
}
