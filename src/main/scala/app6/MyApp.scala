package app6

import cats.Id
import cats.data.EitherK
import cats.free.Free

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

object MyApp extends App {

  import inout.dsl._
  import inout.interpreter._
  import logic.dsl._
  import logic.interpreter._
  import model.Cat

  type AppDSL[A] = EitherK[CatManagement, Inout, A]

  def prog1(implicit io: Inouts[AppDSL],
            cm: CatOps[AppDSL]): Free[AppDSL, Option[Cat]] = {
    for {
      name <- io.ask("Cat's name?")
      age <- io.ask("Cat's age?")
      cat <- cm.create(Cat(name, age.toInt))
      newAge <- io.ask("That was a lie! Tell me the correct age!")
      _ <- cm.updateById(cat.copy(age = newAge.toInt))
      _ <- io.printline(s"Hello cat ${cat.name}! Your age is ${cat.age}!")
      optCat <- cm.findById(cat.id)
    } yield optCat
  }

  def askCatAndSave(implicit io: Inouts[AppDSL],
                    cm: CatOps[AppDSL]): Free[AppDSL, Cat] = {
    for {
      name <- io.ask("Cat's name?")
      age <- io.ask("Cat's age?")
      cat = Cat(name, age.toInt)
      newCat <- cm.create(cat)
    } yield newCat
  }

  def prog2(implicit io: Inouts[AppDSL],
            cm: CatOps[AppDSL]): Free[AppDSL, List[Cat]] = {
    for {
      cat1 <- askCatAndSave
      cat2 <- askCatAndSave
      cat3 <- askCatAndSave
      _ <- cm.findAll.map { found => println(found); found }
      // _ <- cm.updateById(cat1.copy(id = 42L.toString, age = cat1.age + 10)) // throws Exeption
      _ <- cm.updateById(cat1.copy(age = cat1.age + 10))
      _ <- io.printline("Garfield updated")
      _ <- cm.deleteById(cat2.id) // 1st delete successfull
      _ <- cm.deleteById(cat2.id) // 2nd delete logs a warning
      _ <- io.printline("Mizzi deleted")
      byName <- cm.findByName(cat3.name)
      _ <- io.printline(s"findByName(${cat3.name}) found ${byName.length} cats.")
      byId1 <- cm.findById(cat1.id)
      _ <- io.printline(s"findById(${cat1.id}) found $byId1.")
      byId2 <- cm.findById(cat2.id)
      _ <- io.printline(s"findById(${cat2.id}) found $byId2.")
      allCats <- cm.findAll
      _ <- io.printline(s"findAll found ${allCats.length} cats.")
    } yield allCats
  }

  // program execution: the program must be combined with an interpreter

  def execProg1Sync(): Unit = {
    println("\n----- Execute program 1 with CatLogicInterpreter or ConsoleInterpreter")
    val result: Id[Option[Cat]] = prog1.foldMap(CatLogicInterpreter or ConsoleInterpreter)
    println(s"result = $result")
  }

//  def execAsync(): Unit = {
//    println("\n----- Execute program 1 with SeqAsyncInterpreter and KVSAsyncInterpreter and AsyncInterpreter")
//
//    import cats.instances.future._ // bring implicit monad instance for Future into scope
//
//    val future: Future[(String, Option[Cat])] = prog1.foldMap(SeqAsyncInterpreter or (KVSAsyncInterpreter or AsyncInterpreter))
//    val result2 = Await.result(future, 15.second)
//    println(s"result2 = $result2")
//  }

  def execProg1Test(): Unit = {

    val inputs = ListBuffer[String]("Garfield", "22")
    val outputs = ListBuffer[String]()

    println("\n----- Execute program 1 with CatLogicInterpreter and TestInterpreter")
    val optCat: Id[Option[Cat]] = prog1.foldMap(CatLogicInterpreter or new TestInterpreter(inputs, outputs))
    println(s"optCat = $optCat")
    println(s"outputs = $outputs")

    println("\n----- Test -----")
    assert(optCat.isDefined)
    val cat = optCat.get
    assert(cat.name == "Garfield" && cat.age == 22)
    println("asserted: result as expected")
    assert(outputs == ListBuffer("Cat's name?", "Cat's age?", "Hello cat Garfield! Your age is 22!"))
    println("asserted: outputs as expected")
  }

  def execProg2Sync(): Unit = {
    println("\n----- Execute program 2 with CatLogicInterpreter or ConsoleInterpreter")
    val result: Id[List[Cat]] = prog2.foldMap(CatLogicInterpreter or ConsoleInterpreter)
    println(s"result = $result")
  }

  def execProg2Test(): Unit = {

    val inputs = ListBuffer[String]("Garfield", "22", "Mizzi", "2", "Mimi", "3")
    val outputs = ListBuffer[String]()

    println("\n----- Execute program 1 with CatLogicInterpreter and TestInterpreter")
    val cats: Id[List[Cat]] = prog2.foldMap(CatLogicInterpreter or new TestInterpreter(inputs, outputs))

    println("\n----- Test -----")
    assert(cats.length == 2)
    val cat1 = cats.head
    val cat2 = cats.tail.head
    assert(cat1.name == "Garfield" && cat1.age == 32)
    assert(cat2.name == "Mimi" && cat2.age == 3)
    println(s"asserted: result as expected: $cats")
    val expectedOutputs = ListBuffer("Cat's name?", "Cat's age?", "Cat's name?", "Cat's age?", "Cat's name?", "Cat's age?",
      "Garfield updated", "Mizzi deleted", "findByName(Mimi) found 1 cats.", "findById(1) found Some(Cat(1,Garfield,32)).", "findById(2) found None.", "findAll found 2 cats.")
    println(s"outputs = \n$outputs")
    //println(s"::: expectedOutputs = \n$expectedOutputs")
    assert(outputs == expectedOutputs)
    println("asserted: outputs as expected")
  }

  // execProg1Sync()
  // execAsync()
  // execProg1Test()
  // execProg2Sync()
  execProg2Test()

  println("\n-----\n")
}
