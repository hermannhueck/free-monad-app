package app5freek.seq

import java.util.concurrent.atomic.AtomicInteger

import cats.{Id, ~>}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object interpreter {

  import dsl._

  object SeqInterpreter extends (Sequence ~> Id) {

    val sequence = new AtomicInteger(0)

    override def apply[A](fa: Sequence[A]): Id[A] = fa match {
      case NextId =>
        val nextId = sequence.incrementAndGet()
        nextId
    }
  }

  object SeqAsyncInterpreter extends (Sequence ~> Future) {

    val sequence = new AtomicInteger(0)

    override def apply[A](fa: Sequence[A]): Future[A] = fa match {
      case NextId => Future {
        val nextId = sequence.incrementAndGet()
        nextId
      }
    }
  }
}
