package app6.logic

import scala.language.higherKinds
import cats.InjectK
import cats.free.Free
import app6.model.Cat

object dsl {

  sealed trait CatManagement[A] extends Product with Serializable
  final case class Create(cat: Cat) extends CatManagement[Cat]
  final case class UpdateById(cat: Cat) extends CatManagement[Cat]
  final case class DeleteById(id: String) extends CatManagement[Boolean]
  final case class FindById(id: String) extends CatManagement[Option[Cat]]
  final case class FindByName(name: String) extends CatManagement[List[Cat]]
  case object FindAll extends CatManagement[List[Cat]]

  class CatOps[F[_]](implicit KV: InjectK[CatManagement, F]) {
    def create(cat: Cat): Free[F, Cat] = Free.inject[CatManagement, F](Create(cat))
    def updateById(cat: Cat): Free[F, Cat] = Free.inject[CatManagement, F](UpdateById(cat))
    def deleteById(id: String): Free[F, Boolean] = Free.inject[CatManagement, F](DeleteById(id))
    def findById(id: String): Free[F, Option[Cat]] = Free.inject[CatManagement, F](FindById(id))
    def findByName(name: String): Free[F, List[Cat]] = Free.inject[CatManagement, F](FindByName(name))
    def findAll: Free[F, List[Cat]] = Free.inject[CatManagement, F](FindAll)
  }

  object CatOps {
    implicit def catOps[F[_]](implicit CM: InjectK[CatManagement, F]): CatOps[F] = new CatOps[F]
  }
}
