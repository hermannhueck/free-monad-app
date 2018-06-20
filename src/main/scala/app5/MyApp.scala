package app5

import cats.data.EitherK

import scala.language.higherKinds
import cats.free.Free
import cats.Id

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  import inout.dsl._
  import inout.interpreter._
  import kvs.dsl._
  import kvs.interpreter._
  import seq.dsl._
  import seq.interpreter._
  import model.Cat

  type AppDSL0[A] = EitherK[KVStore, Inout, A]
  type AppDSL[A] = EitherK[Sequence, AppDSL0, A]

  // program definition (does nothing)

  def prog(implicit io: Inouts[AppDSL],
                    kvs: KVSOps[AppDSL],
                    seq: SeqOps[AppDSL]): Free[AppDSL, (String, Option[Cat])] = {
    for {
      name <- io.ask("What's your name?")
      age <- io.ask("What's your age?")
      id <- seq.nextId.map(_.toString)
      _ <- kvs.put(id, Cat(id, name, age.toInt))
      _ <- io.printline(s"Hello cat $name! Your age is $age!")
      optCat <- kvs.get(id)
    } yield (id, optCat)
  }

  // program execution: the program must be combined with an interpreter

  def execSync(): Unit = {
    println("\n----- Execute program with SeqInterpreter and KVSSyncInterpreter and ConsoleInterpreter")
    val result1: Id[(String, Option[Cat])] = prog.foldMap(SeqInterpreter or (KVSSyncInterpreter or ConsoleInterpreter))
    println(s"result1 = $result1")
  }

  def execAsync(): Unit = {
    println("\n----- Execute program with SeqAsyncInterpreter and KVSAsyncInterpreter and AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val future: Future[(String, Option[Cat])] = prog.foldMap(SeqAsyncInterpreter or (KVSAsyncInterpreter or AsyncInterpreter))
    val result2 = Await.result(future, 15.second)
    println(s"result2 = $result2")
  }

  def execTest(): Unit = {
    println("\n----- Execute program with SeqInterpreter and KVSSyncInterpreter and TestInterpreter")
    val inputs = ListBuffer[String]("Garfield", "22")
    val outputs = ListBuffer[String]()
    val result3: Id[(String, Option[Cat])] = prog.foldMap(SeqInterpreter or (KVSSyncInterpreter or new TestInterpreter(inputs, outputs)))
    println(s"result3 = $result3")
    println(s"outputs = $outputs")
    // Test:
    val optCat = result3._2
    assert(optCat.isDefined)
    val cat = optCat.get
    assert(cat.name == "Garfield" && cat.age == 22)
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello cat Garfield! Your age is 22!"))
    println("asserted outputs ok")
  }

  // execSync()
  // execAsync()
  execTest()

  println("\n-----\n")
}
