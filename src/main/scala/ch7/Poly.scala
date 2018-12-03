package ch7

import shapeless._
import Poly._
import shapeless.ops.hlist

import scala.math.Numeric
import scala.math.Numeric.Implicits._

object Poly {
  object myPoly extends Poly1 {
    implicit val intCase: Case.Aux[Int, Double] = at(_ / 2.0)

    implicit val stringCase: Case.Aux[String, Int] = at(_.length)
  }

  object multiply extends Poly2 {
    implicit val iniIntCase: Case.Aux[Int, Int, Int] =
      at(_ * _)

    implicit val iniStrCase: Case.Aux[Int, String, String] =
      at((a, b) => b * a)

  }

  object total extends Poly1 {
    implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] =
      at(_.toDouble())

    implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] =
      at(opt => opt.map(num.toDouble).getOrElse(0))

    implicit def list[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] =
      at(list => num.toDouble(list.sum))
  }

  object sizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int] =
      at(identity)

    implicit val stringCase: Case.Aux[String, Int] =
      at(_.length)

    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(b => if(b) 1 else 0)
  }

  object valueAndSizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
      at(num => num :: num :: HNil)

    implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
      at(str => str :: str.length :: HNil)

    implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
      at(b => b :: (if(b) 1 else 0) :: HNil)
  }

  object sum extends Poly2 {
    implicit val intIntCase: Case.Aux[Int, Int, Int] =
      at((a, b) => a + b)

    implicit val intStringCase: Case.Aux[Int, String, Int] =
      at((a, b) => a + b.length)
  }

  trait ProductMapper[A, B, P] {
    def apply(a: A): B
  }

  implicit def genericProductMapper[
  A, B,
  P <: Poly,
  ARepr <: HList,
  BRepr <: HList
  ](
    implicit
    aGen: Generic.Aux[A, ARepr],
    bGen: Generic.Aux[B, BRepr],
    mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] =
    a => bGen.from(mapper(aGen.to(a)))

  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
        pm(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }

  object conversions extends Poly1 {
    implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
    implicit val boolCase: Case.Aux[Boolean, Int] = at(if(_) 1 else 0)
    implicit val strCase: Case.Aux[String, String] = at(identity)
  }

  case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)
}

object PolyMain extends App {
  println(myPoly(123))
  println(myPoly("hello"))

  println(multiply(3, 4))
  println(multiply(3, "4"))

  println(total(10))
  println(total(Option(20.4)))

  println(total(List(1L, 2L, 3L)))

  println((10 :: "Hello" :: true :: HNil).map(sizeOf))
  println((10 :: "Hello" :: true :: HNil).flatMap(valueAndSizeOf))

  println((10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum))

  println(IceCream1("Sundae", 1, inCone = false).mapTo[IceCream2](conversions))
}
