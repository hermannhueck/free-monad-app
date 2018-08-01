package app6.model

final case class Cat(id: String, name: String, age: Int)

object Cat {
  def apply(name: String, age: Int): Cat = Cat((-1L).toString, name, age)
}
