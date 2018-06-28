package myfreeimpl

import cats.{InjectK, Monad}

import scala.language.higherKinds

trait ~>[F[_], G[_]] {

  def apply[A](fa: F[A]): G[A]
}

sealed trait Free[F[_], A] {

  import Free._

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(a => pure(f(a)))

  def foldMap[G[_]: Monad](nt: F ~> G): G[A] = this match {
    case Pure(a) => Monad[G].pure(a)
    case Suspend(fa) => nt(fa)
    case FlatMap(target, f) =>
      Monad[G].flatMap(target.foldMap(nt)) { e =>
        f(e).foldMap(nt)
      }
  }
}

object Free {

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  case class FlatMap[F[_], E, A](target: Free[F, E], f: E => Free[F, A]) extends Free[F, A]

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

/*
  def inject[F[_], G[_]]: FreeInjectKPartiallyApplied[F, G] = new FreeInjectKPartiallyApplied

  private[myfree] final class FreeInjectKPartiallyApplied[F[_], G[_]](val dummy: Boolean = true ) extends AnyVal {
    def apply[A](fa: F[A])(implicit I: InjectK[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))
  }
*/
}
