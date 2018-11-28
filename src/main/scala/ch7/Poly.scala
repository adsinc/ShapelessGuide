package ch7

import shapeless._
import Poly._

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
}

object PolyMain extends App {
  println(myPoly(123))
  println(myPoly("hello"))

  println(multiply(3, 4))
  println(multiply(3, "4"))
}
