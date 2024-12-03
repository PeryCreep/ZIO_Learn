package variance

trait Animal {
  val name: String
}

trait PatableAnimal extends Animal {
  def pet(): Unit
}

case class Cat(name: String) extends PatableAnimal {
  override def pet(): Unit = println(s"Cat with name $name was patted")
}

case class Dog(name: String) extends PatableAnimal {
  override def pet(): Unit = println(s"Dog with name $name was patted")
}

trait PatAnimalProcessor[A] {
  def patAnimal(animal: A): Unit
}

case class CatPatProcessor[Cat]() extends PatAnimalProcessor[Cat] {
  override def patAnimal(animal: Cat): Unit = ???
}

case class DogPatProcessor[Dog]() extends PatAnimalProcessor[Dog] {
  override def patAnimal(animal: Dog): Unit = ???
}

case class AnimalPatProcessor[Animal]() extends PatAnimalProcessor[Animal] {
  override def patAnimal(animal: Animal): Unit = ???
}

case class PatAnimalPatProcessor[PatableAnimal]() extends PatAnimalProcessor[PatableAnimal] {
  override def patAnimal(animal: PatableAnimal): Unit = ???
}


