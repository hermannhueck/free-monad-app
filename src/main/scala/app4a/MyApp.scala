package app4a

import scala.language.higherKinds
import scala.language.existentials

import cats.arrow.FunctionK
import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  object inout {

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
        implicit def ioOps[F[_]](implicit IO: InjectK[Inout, F]): IoOps[F] = new IoOps[F]
      }
    }

    object interpreter {

      import dsl._

      object ConsoleInterpreter extends (Inout ~> Id) {

        override def apply[A](fa: Inout[A]): Id[A] = fa match {
          case Printline(out) =>
            println(out)
            (): Id[Unit]
          case Getline =>
            val in = scala.io.StdIn.readLine()
            in: Id[String]
        }
      }

      object AsyncInterpreter extends (Inout ~> Future) {

        override def apply[A](fa: Inout[A]): Future[A] = fa match {
          case Printline(out) => Future {
            println(out)
          }
          case Getline => Future {
            scala.io.StdIn.readLine()
          }
        }
      }

      class TestInterpreter(inputs: ListBuffer[String], outputs: ListBuffer[String]) extends (Inout ~> Id) {

        override def apply[A](fa: Inout[A]): Id[A] = fa match {
          case Printline(out) =>
            outputs append out
            (): Id[Unit]
          case Getline =>
            val in = inputs.head
            inputs -= inputs.head
            in: Id[String]
        }
      }
    }
  }

  object kvs {

    object dsl {

      trait KVStore[A]
      final case class Put(key: String, value: Int) extends KVStore[Unit]
      final case class Get(key: String) extends KVStore[Option[Int]]
      final case class Delete(key: String) extends KVStore[Option[Int]]

      class KVSOps[F[_]](implicit KV: InjectK[KVStore, F]) {
        def put(key: String, value: Int): Free[F, Unit] = Free.inject[KVStore, F](Put(key: String, value: Int))
        def get(key: String): Free[F, Option[Int]] = Free.inject[KVStore, F](Get(key: String))
        def delete(key: String): Free[F, Option[Int]] = Free.inject[KVStore, F](Delete(key: String))
      }

      object KVSOps {
        implicit def kvsOps[F[_]](implicit IO: InjectK[KVStore, F]): KVSOps[F] = new KVSOps[F]
      }
    }

    object interpreter {

      import dsl._

      object KVSInterpreter extends (dsl.KVStore ~> Id) {

        var kvs: Map[String, Int] = Map.empty

        override def apply[A](fa: KVStore[A]): Id[A] = fa match {
          case Put(key, value) =>
            kvs = kvs.updated(key, value)
            (): Id[Unit]
          case Get(key) =>
            kvs.get(key): Id[Option[Int]]
          case Delete(key) =>
            val value = kvs.get(key)
            kvs = kvs - key
            value: Id[Option[Int]]
        }
      }

      object KVSAsyncInterpreter extends (dsl.KVStore ~> Future) {

        var kvs: Map[String, Int] = Map.empty

        override def apply[A](fa: KVStore[A]): Future[A] = fa match {
          case Put(key, value) => Future {
            kvs = kvs.updated(key, value)
          }
          case Get(key) => Future {
            kvs.get(key)
          }
          case Delete(key) => Future {
            val value = kvs.get(key)
            kvs = kvs - key
            value
          }
        }
      }
    }
  }

  import inout.dsl._
  import inout.interpreter._
  import kvs.dsl._
  import kvs.interpreter._

  type AppDSL[A] = EitherK[Inout, KVStore, A]


  // --- 1 ---

  def execTest1(): Unit = {

    // This is the usual way to define a program.
    // The program receives the partial DSLs (InoutOps and KVSOps) as implicit parameters.
    // InoutOps and KVSOps are define aboce before AppDSL is defined.
    //
    // In the following steps I will make the composition process more understandable
    // by defining the composed DSL after AppDSL[A] is defined as alias of EitherK[Inout, KVStore, A]

    def prog(implicit io: IoOps[AppDSL], kvs: KVSOps[AppDSL]): Free[AppDSL, (String, Option[Int])] = {
      for {
        name <- io.ask("What's your name?")
        age <- io.ask("What's your age?")
        _ <- kvs.put(name, age.toInt)
        _ <- io.printline(s"Hello $name! Your age is $age!")
        optAge <- kvs.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog1 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: AppDSL ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest1()


  // --- 2 ---

  // Here I define the composed DSL in one class.
  // Normally we implements this in two different classes what gives us better composability of the DSLs.
  // I pass the two necessary instances of InjectK (IO and KV) as implicit parameters to the class constructor.
  // The implementation of the DSL methods (printline, getline, ask, put, get, delete) is exactly the same as above
  // in InoutOps and KVSOps. It is just merged into the ComposedDSL.
  // ComposedDSL has still that that type constructor parameter F which is resolved to AppDSL
  // when an instance of ComposedDSL is constructed.

  def execTest2(): Unit = {

    class ComposedDSL[F[_]](implicit IO: InjectK[Inout, F], KV: InjectK[KVStore, F]) {

      def printline(out: String): Free[F, Unit] = Free.inject[Inout, F](Printline(out))
      def getline: Free[F, String] = Free.inject[Inout, F](Getline)
      def ask(prompt: String): Free[F, String] = for {
        _ <- printline(prompt)
        input <- getline
      } yield input

      def put(key: String, value: Int): Free[F, Unit] = Free.inject[KVStore, F](Put(key: String, value: Int))
      def get(key: String): Free[F, Option[Int]] = Free.inject[KVStore, F](Get(key: String))
      def delete(key: String): Free[F, Option[Int]] = Free.inject[KVStore, F](Delete(key: String))
    }

    implicit val composedDSL: ComposedDSL[AppDSL] = new ComposedDSL[AppDSL]

    def prog(implicit dsl: ComposedDSL[AppDSL]): Free[AppDSL, (String, Option[Int])] = {
      for {
        name <- dsl.ask("What's your name?")
        age <- dsl.ask("What's your age?")
        _ <- dsl.put(name, age.toInt)
        _ <- dsl.printline(s"Hello $name! Your age is $age!")
        optAge <- dsl.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog2 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: AppDSL ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest2()


  // --- 3 ---

  def execTest3(): Unit = {

    // In this implementation step I removed the type parameter F[_] and replaced F by AppDSL in the impl of the class.
    // The InjectK instances become InjectK[Inout, AppDSL] and InjectK[KVStore, AppDSL]
    // instead of InjectK[Inout, F] and InjectK[KVStore, F].

    // Additionally I resolved all occurences of Free.inject(fa) by it's implementation: Free.liftF(I.inj(fa))
    //
    // def Free.inject[F[_], G[_], A](fa: F[A])(implicit I: InjectK[F, G]): Free[G, A] =     // simplified
    //          Free.liftF(I.inj(fa))
    //
    // We can see that inject invokes liftF after injection is completed.

    class ComposedDSL(implicit IO: InjectK[Inout, AppDSL], KV: InjectK[KVStore, AppDSL]) {

      def printline(out: String): Free[AppDSL, Unit] = Free.liftF(IO.inj(Printline(out)))
      def getline: Free[AppDSL, String] = Free.liftF(IO.inj(Getline))
      def ask(prompt: String): Free[AppDSL, String] = for {
        _ <- printline(prompt)
        input <- getline
      } yield input

      def put(key: String, value: Int): Free[AppDSL, Unit] = Free.liftF(KV.inj(Put(key: String, value: Int)))
      def get(key: String): Free[AppDSL, Option[Int]] = Free.liftF(KV.inj(Get(key: String)))
      def delete(key: String): Free[AppDSL, Option[Int]] = Free.liftF(KV.inj(Delete(key: String)))
    }

    implicit val composedDSL: ComposedDSL = new ComposedDSL

    def prog(implicit dsl: ComposedDSL): Free[AppDSL, (String, Option[Int])] = {
      for {
        name <- dsl.ask("What's your name?")
        age <- dsl.ask("What's your age?")
        _ <- dsl.put(name, age.toInt)
        _ <- dsl.printline(s"Hello $name! Your age is $age!")
        optAge <- dsl.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog3 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: AppDSL ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest3()


  // --- 4 ---

  // In step 4 I removed the implicit parameters IO and KV.
  // Instead I construct them myself by implementing their abstract methods inj() and proj().
  // proj() is implemented with ??? as it is not needed here.
  // The method inj() is more interesting and will be used in our case:
  //          (remember: type AppDSL[A] = EitherK[Inout, KVStore, A]
  //
  // IO.inj() constructs a natural transformation FunktionK from Inout ~> AppDSL
  // It finally turns an Inout into an EitherK(Left(fa))
  //
  // KV.inj() constructs a natural transformation FunktionK from KVStore ~> AppDSL
  // It finally turns an KVStore into an EitherK(Right(fa))

  def execTest4(): Unit = {

    class ComposedDSL {

      def IO[A]: InjectK[Inout, AppDSL] = new InjectK[Inout, AppDSL] {
        override def inj: Inout ~> AppDSL = new FunctionK[Inout, AppDSL] {
          override def apply[A](fa: Inout[A]): AppDSL[A] = EitherK(Left(fa))
        }

        def prj: FunctionK[AppDSL, Lambda[X => Option[Inout[X]]]] = ???
      }

      def KV[A]: InjectK[KVStore, AppDSL] = new InjectK[KVStore, AppDSL] {
        override def inj: KVStore ~> AppDSL = new FunctionK[KVStore, AppDSL] {
          override def apply[B](fb: KVStore[B]): AppDSL[B] = EitherK(Right(fb))
        }

        def prj: FunctionK[AppDSL, ({ type OptKVS[X] = Option[KVStore[X]] })#OptKVS ] = ???
      }

      def printline(out: String): Free[AppDSL, Unit] = Free.liftF(IO.inj(Printline(out)))
      def getline: Free[AppDSL, String] = Free.liftF(IO.inj(Getline))
      def ask(prompt: String): Free[AppDSL, String] = for {
        _ <- printline(prompt)
        input <- getline
      } yield input

      def put(key: String, value: Int): Free[AppDSL, Unit] = Free.liftF(KV.inj(Put(key: String, value: Int)))
      def get(key: String): Free[AppDSL, Option[Int]] = Free.liftF(KV.inj(Get(key: String)))
      def delete(key: String): Free[AppDSL, Option[Int]] = Free.liftF(KV.inj(Delete(key: String)))
    }

    implicit val composedDSL: ComposedDSL = new ComposedDSL

    def prog(implicit dsl: ComposedDSL): Free[AppDSL, (String, Option[Int])] = {
      for {
        name <- dsl.ask("What's your name?")
        age <- dsl.ask("What's your age?")
        _ <- dsl.put(name, age.toInt)
        _ <- dsl.printline(s"Hello $name! Your age is $age!")
        optAge <- dsl.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog4 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: AppDSL ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest4()


  // --- 5 ---

  // Replacing the injections IO.inj(...) and KV.inj(...) by their implementation from step 4
  // we now can easily see that each implementation of a DSL methods
  // Free.liftF()s an EitherK(Left(adt) or an EitherK(Right(adt) into the Free Monad
  // where 'adt' is an instance of the respective ADT trait like Printline, Getline, Put ...

  def execTest5(): Unit = {

    class ComposedDSL {

      def printline(out: String): Free[AppDSL, Unit] = Free.liftF(EitherK(Left(Printline(out))))
      def getline: Free[AppDSL, String] = Free.liftF(EitherK(Left(Getline)))
      def ask(prompt: String): Free[AppDSL, String] = for {
        _ <- printline(prompt)
        input <- getline
      } yield input

      def put(key: String, value: Int): Free[AppDSL, Unit] = Free.liftF(EitherK(Right(Put(key: String, value: Int))))
      def get(key: String): Free[AppDSL, Option[Int]] = Free.liftF(EitherK(Right(Get(key: String))))
      def delete(key: String): Free[AppDSL, Option[Int]] = Free.liftF(EitherK(Right(Delete(key: String))))
    }

    implicit val composedDSL: ComposedDSL = new ComposedDSL

    def prog(implicit dsl: ComposedDSL): Free[AppDSL, (String, Option[Int])] = {
      for {
        name <- dsl.ask("What's your name?")
        age <- dsl.ask("What's your age?")
        _ <- dsl.put(name, age.toInt)
        _ <- dsl.printline(s"Hello $name! Your age is $age!")
        optAge <- dsl.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog5 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: AppDSL ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest5()


  // --- 6 ---

  // Here I resolve the type alias AppDSL again to it's definition just to show, what Free.inject()
  // really returns: an EitherK[Inout, KVStore, A] lifted into the Free Monad: Free[EitherK[Inout, KVStore, ?], A]
  // where A is the result type of the respective method.

  def execTest6(): Unit = {

    class ComposedDSL {

      def printline(out: String): Free[EitherK[Inout, KVStore, ?], Unit] = Free.liftF(EitherK(Left(Printline(out))))
      def getline: Free[EitherK[Inout, KVStore, ?], String] = Free.liftF(EitherK(Left(Getline)))
      def ask(prompt: String): Free[EitherK[Inout, KVStore, ?], String] = for {
        _ <- printline(prompt)
        input <- getline
      } yield input

      def put(key: String, value: Int): Free[EitherK[Inout, KVStore, ?], Unit] = Free.liftF(EitherK(Right(Put(key: String, value: Int))))
      def get(key: String): Free[EitherK[Inout, KVStore, ?], Option[Int]] = Free.liftF(EitherK(Right(Get(key: String))))
      def delete(key: String): Free[EitherK[Inout, KVStore, ?], Option[Int]] = Free.liftF(EitherK(Right(Delete(key: String))))
    }

    implicit val composedDSL: ComposedDSL = new ComposedDSL

    def prog(implicit dsl: ComposedDSL): Free[EitherK[Inout, KVStore, ?], (String, Option[Int])] = {
      for {
        name <- dsl.ask("What's your name?")
        age <- dsl.ask("What's your age?")
        _ <- dsl.put(name, age.toInt)
        _ <- dsl.printline(s"Hello $name! Your age is $age!")
        optAge <- dsl.get(name)
      } yield (name, optAge)
    }

    println("\n----- Execute prog6 with KVSInterpreter and TestInterpreter")

    val inputs = ListBuffer[String]("John Doe", "33")
    val outputs = ListBuffer[String]()
    val composedInterpreter: EitherK[Inout, KVStore, ?] ~> Id =
      (new TestInterpreter(inputs, outputs): Inout ~> Id) or (KVSInterpreter: KVStore ~> Id)

    val result: Id[(String, Option[Int])] = prog.foldMap(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
  }
  execTest6()

  println("\n-----\n")
}
