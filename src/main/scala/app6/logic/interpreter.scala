package app6.logic

import scala.language.higherKinds

import cats.data.EitherK
import cats.free.Free
import cats.{Id, ~>}

import app6.kvs.dsl._
import app6.kvs.interpreter._
import app6.logic.dsl._
import app6.seq.dsl._
import app6.seq.interpreter._
import app6.log.dsl._
import app6.log.interpreter._
import app6.model.Cat

object interpreter {


  // ----- composition of DSLs and interpreters

  // compose DSLs
  type ComposedDSL0[A] = EitherK[Sequence, KVStore, A]
  type ComposedDSL[A] = EitherK[Log, ComposedDSL0, A]

  // compose interpreters
  val ComposedLogSeqKvsInterpreter: ComposedDSL ~> Id =
            LogInterpreter or (SeqInterpreter or KvsInterpreter)


  // type definition for the Free Monad of the composed DSL
  type FreeComposed[A] = Free[ComposedDSL, A]

  // interpreter that translated from the composed Free Monad to the Id Monad
  object FreeComposedLogSeqKvsInterpreter extends (FreeComposed ~> Id) {
    override def apply[A](fa: FreeComposed[A]): Id[A] = fa.foldMap(ComposedLogSeqKvsInterpreter)
  }

  // CatLogicInterpreter provides a natural transformation (CatManagement ~> Id).
  // It transforms (CatManagement ~> FreeComposed) 'andThen' propagates to FreeComposedInterpreter
  // which transforms (FreeComposed ~> Id).

  // Routing with FunctionK.andThen
  val CatLogicInterpreter: CatManagement ~> Id = new CatLogicInterpreter andThen FreeComposedLogSeqKvsInterpreter


  // ----- CatLogicInterpreter transforms from CatManagement ~> FreeComposed
  // ----- and is implemented with the DSL composed from Logging, Sequence and KVStore

  class CatLogicInterpreter(implicit log: LogOps[ComposedDSL],
                            seq: SeqOps[ComposedDSL],
                            kvs: KVSOps[ComposedDSL]) extends (CatManagement ~> FreeComposed) {

    override def apply[A](fa: CatManagement[A]): FreeComposed[A] = fa match {

      case Create(cat) =>
        kvsCreate(cat): FreeComposed[Cat]
      case UpdateById(cat) =>
        kvsUpdateById(cat): FreeComposed[Cat]
      case DeleteById(id) =>
        kvsDeleteById(id): FreeComposed[Boolean]
      case FindById(id) =>
        kvsFindById(id): FreeComposed[Option[Cat]]
      case FindByName(name) =>
        kvsFindByName(name): FreeComposed[List[Cat]]
      case FindAll =>
        kvsFindAll: FreeComposed[List[Cat]]
    }

    private def kvsCreate[A](cat: Cat): FreeComposed[Cat] =
      for {
        maybeCat <- kvs.get(cat.id)
        _ = if (maybeCat.isDefined) {
          val message = s"cat with id ${cat.id} already exists"
          log.error(message)
          throw new RuntimeException(message)
        }
        newId <- seq.nextStringId
        _ <- kvs.put(newId, cat.copy(id = newId))
        newMaybeCat <- kvs.get(newId)
        _ <- log.info(s"Created: $cat")
      } yield newMaybeCat.get

    private def kvsUpdateById[A](cat: Cat): FreeComposed[Cat] =
      for {
        optCat <- kvs.get(cat.id)
        _ = if (optCat.isEmpty) {
          val message = s"cat with id ${cat.id} does not exist for update"
          log.error(message)
          throw new RuntimeException(message)
        }
        _ <- kvs.put(cat.id, cat)
        newCatOpt <- kvs.get(cat.id)
      } yield newCatOpt.get

    private def kvsDeleteById[A](id: String): FreeComposed[Boolean] =
      for {
        optCat <- kvs.get(id)
        notFound = optCat.isEmpty
        _ <- if (notFound) {
          log.warn(s"cat with id $id not found for deletion ")
        } else for {
          _ <- kvs.delete(id)
          _ <- log.info(s"cat with id $id deleted")
        } yield ()
      } yield !notFound

    private def kvsFindById[A](id: String): FreeComposed[Option[Cat]] =
      kvs.get(id)

    private def kvsFindByName[A](name: String): FreeComposed[List[Cat]] =
      kvs.getAll.map(_.filter(_.name == name))

    private def kvsFindAll[A]: FreeComposed[List[Cat]] =
      kvs.getAll
  }

}
