package app6.seq

import scala.language.higherKinds

import cats.InjectK
import cats.free.Free

object dsl {

  sealed trait Sequence[A] extends Product with Serializable
  case object NextId extends Sequence[Long]

  class SeqOps[F[_]](implicit SQ: InjectK[Sequence, F]) {
    def nextId: Free[F, Long] = Free.inject[Sequence, F](NextId)
    def nextStringId: Free[F, String] = nextId.map(_.toString)
  }

  object SeqOps {
    implicit def seqOps[F[_]](implicit SQ: InjectK[Sequence, F]): SeqOps[F] = new SeqOps[F]
  }
}
