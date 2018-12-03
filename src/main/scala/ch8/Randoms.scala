package ch8

import Randoms._

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

  implicit val charRandom: Random[Char] = createRandom(() => scala.util.Random.nextInt(26).toChar)

  implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean())

}

object RandomMain extends App {
  for(_ <- 1 to 3) println(random[Int])
  for(_ <- 1 to 3) println(random[Char])
}
