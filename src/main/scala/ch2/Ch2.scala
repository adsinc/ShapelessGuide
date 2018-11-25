package ch2

import shapeless.{HList, ::, HNil}

object Ch2 {

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
}
