package ch5

import shapeless._
import shapeless.record._
import ch2.Ch2.IceCream

object Records {

}

object RecordsMain extends App {

  val sundae = LabelledGeneric[IceCream].to(IceCream("Sundae", 1, inCone = false))
  println(sundae)

  println(sundae.get('name))
  println(sundae.get('numCherries))

  println(sundae.updated('numCherries, 2))
  println(sundae.updateWith('name)("PREF_" + _))

  println(sundae.toMap)

}