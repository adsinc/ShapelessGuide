package ch8

import ch2.Ch2.IceCream
import ch8.Ch8._
import shapeless._
import shapeless.ops.{coproduct, hlist, nat}

object Ch8 {
  val hlistLenght = hlist.Length[String :: Int :: Boolean :: HNil]
  val coproductLenght = coproduct.Length[Double :+: Char :+: CNil]

  trait SizeOf[A] {
    def value: Int
  }

  def sizeOf[A](implicit size: SizeOf[A]): Int =
    size.value

  implicit def genericSizeOf[A, L <: HList, N <: Nat](
    implicit
    generic: Generic.Aux[A, L],
    size: hlist.Length.Aux[L, N],
    sizeToInt: nat.ToInt[N]
  ): SizeOf[A] =
    new SizeOf[A] {
      val value: Int = sizeToInt.apply
    }
}

object Main extends App {
  println(Nat.toInt[hlistLenght.Out])
  println(Nat.toInt[coproductLenght.Out])

  println(sizeOf[IceCream])
}