package app5freek

import scala.language.higherKinds

import cats.free.Free
import cats.Id
import freek._

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

  // program definition (does nothing)

  type AppDSL = Inout :|: KVStore :|: Sequence :|: NilDSL
  val appDSL = DSL.Make[AppDSL]

  def prog: Free[appDSL.Cop, (String, Option[Cat])] = {
    for {
      name <- ask("What's your name?").expand[AppDSL]
      age <- ask("What's your age?").expand[AppDSL]
      idLong <- NextId.freek[AppDSL]
      id = idLong.toString
      _ <- Put(id, Cat(id, name, age.toInt)).freek[AppDSL]
      _ <- Printline(s"Hello cat $name! Your age is $age!").freek[AppDSL]
      optCat <- Get(id).freek[AppDSL]
    } yield (id, optCat)
  }

  // program execution: the program must be combined with an interpreter

  def execSync(): Unit = {
    println("\n----- Execute program with SeqInterpreter and KVSInterpreter and ConsoleInterpreter")
    // program execution with foldMap or with interpret
    val composedInterpreter = ConsoleInterpreter :&: KVSInterpreter :&: SeqInterpreter
    val result1: Id[(String, Option[Cat])] = prog.foldMap(composedInterpreter.nat) // foldMap is order-sensitive
    println(s"result1 = $result1\n")
    val result2: Id[(String, Option[Cat])] = prog.interpret(composedInterpreter) // interpret is order-agnostic
    println(s"result2 = $result2")
  }
  //execSync()

  def execAsync(): Unit = {
    println("\n----- Execute program with SeqAsyncInterpreter and KVSAsyncInterpreter and AsyncInterpreter")

    import cats.instances.future._ // bring implicit monad instance for Future into scope

    val composedInterpreter = SeqAsyncInterpreter :&: KVSAsyncInterpreter :&: AsyncInterpreter
    val future: Future[(String, Option[Cat])] = prog.interpret(composedInterpreter)
    val result = Await.result(future, 15.second)
    println(s"result = $result")
  }
  //execAsync()

  def execTest(): Unit = {
    println("\n----- Execute program with SeqInterpreter and KVSInterpreter and TestInterpreter")
    val inputs = ListBuffer[String]("Garfield", "22")
    val outputs = ListBuffer[String]()
    val composedInterpreter = SeqInterpreter :&: KVSInterpreter :&: new TestInterpreter(inputs, outputs)
    val result: Id[(String, Option[Cat])] = prog.interpret(composedInterpreter)
    println(s"result = $result")
    println(s"outputs = $outputs")
    // Test:
    val optCat = result._2
    assert(optCat.isDefined)
    val cat = optCat.get
    assert(cat.name == "Garfield" && cat.age == 22)
    println("asserted result ok")
    assert(outputs == ListBuffer("What's your name?", "What's your age?", "Hello cat Garfield! Your age is 22!"))
    println("asserted outputs ok")
  }
  execTest()

  println("\n-----\n")
}
