package derivation

import shapeless._

import scala.util.Random

sealed trait Animal
case class Cat(name: String, fish: Int) extends Animal
case class Dog(name: String, bones: Int) extends Animal

object Arbitrary {
  def apply[T](implicit a: Arbitrary[T]) = a

  implicit val arbitraryHNil: Arbitrary[HNil] = new Arbitrary[HNil] {
    def value = HNil
  }
  implicit def arbitraryHCon[H, T <: HList](
   implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :: T] = new Arbitrary[H :: T] {
    def value = h.value :: t.value
  }
  implicit val arbitraryCNil: Arbitrary[CNil] = new Arbitrary[CNil] {
    def value = ???
  }
  implicit def arbitraryCCon[H, T <: Coproduct](
   implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :+: T] = new Arbitrary[H :+: T] {
    def value = if (Random.nextBoolean) Inl(h.value) else Inr(t.value)
  }
  implicit val arbitraryInt: Arbitrary[Int] = new Arbitrary[Int] {
    def value = 1
  }
  implicit val arbitraryString: Arbitrary[String] = new Arbitrary[String] {
    def value = "whatever"
  }

  implicit def arbitraryGen[T, R](implicit
    gen: Generic.Aux[T, R],
    arbitraryRepr: Arbitrary[R]): Arbitrary[T] = new Arbitrary[T] {
      def value: T = gen.from(arbitraryRepr.value)
  }
}

trait Arbitrary[T] {
  def value: T
}

object Demo extends App {
  for(i <- 1 to 10) println(Arbitrary[Animal].value)
}