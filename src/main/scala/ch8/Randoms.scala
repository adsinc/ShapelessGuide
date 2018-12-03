package ch8

import Randoms._
import shapeless._

object Randoms {

  trait Random[A] {
    def get: A
  }

  def random[A](implicit r: Random[A]): A = r.get

  def createRandom[A](func: () => A): Random[A] =
    new Random[A] {
      def get: A = func()
    }

  implicit val intRandom: Random[Int] = createRandom(() => scala.util.Random.nextInt(10))

  implicit val charRandom: Random[Char] = createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

  implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean())

  implicit def genericRandom[A, R](
    implicit
    gen: Generic.Aux[A, R],
    random: Lazy[Random[R]]
  ): Random[A] =
    createRandom(() => gen.from(random.value.get))

  implicit val hnilRandom: Random[HNil] =
    createRandom(() => HNil)

  implicit def hlistRandom[H, T <: HList](
    implicit
    hRandom: Lazy[Random[H]],
    tRandom: Random[T]
  ): Random[H :: T] =
    createRandom(() => hRandom.value.get :: tRandom.get)

  case class Cell(col: Char, row: Int)
}

object RandomMain extends App {
  for(_ <- 1 to 3) println(random[Int])
  for(_ <- 1 to 3) println(random[Char])

  for(_ <- 1 to 5) println(random[Cell])
}
