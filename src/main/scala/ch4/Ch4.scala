package ch4

import shapeless.ops.hlist.{IsHCons, Last}
import shapeless.{::, Generic, HList, HNil}
import Ch4._
import ch4.Ch4.Second.Aux

object Ch4 {
  val last1 = Last[String :: Int :: HNil]

  val last2 = Last[Int :: String :: HNil]

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }

    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
  }

  implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): B =
        value.tail.head
    }

  val second1 = Second[String :: Boolean :: Int :: HNil]

  val second2 = Second[String :: Int :: Boolean :: HNil]

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  def getWrappedValue[A, Repr <: HList, Head](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(input).head

  case class Wrapper(value: Int)
}

object Main extends App {

  println(last1("foo" :: 123 :: HNil))

  println(last2(123 :: "bar" :: HNil))

  println(second1("foo" :: true :: 123 :: HNil))

  println(second2("bar" :: 321 :: false :: HNil))

  println(lastField(Rect(Vec(1, 2), Vec(3, 4))))

  println(getWrappedValue(Wrapper(42)))
}