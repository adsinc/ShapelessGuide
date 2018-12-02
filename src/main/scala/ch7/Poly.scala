package ch7

import shapeless._
import Poly._

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
}
