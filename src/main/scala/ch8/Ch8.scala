package ch8

import ch8.Ch8.Two
import shapeless._
import shapeless.ops.nat.ToInt

object Ch8 {
  type Zero = Nat._0
  type One = Succ[Zero]
  type Two = Succ[One]
}

object Main extends App {
  println(ToInt[Two].apply())
}