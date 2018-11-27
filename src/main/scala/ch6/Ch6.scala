package ch6

import ch6.Ch6.{IceCreamV1, IceCreamV2a, IceCreamV2b, IceCreamV2c}
import shapeless._
import shapeless.ops.hlist

object Ch6 {

  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

  case class IceCreamV2a(name: String, inCone: Boolean)
  case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
  case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

  trait Migration[A, B] {
    def apply(a: A): B
  }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  implicit def genericMigration[
  A, B,
  ARepr <: HList, BRepr <: HList,
  Unaligned <: HList
  ](
    implicit
    aGen: LabelledGeneric.Aux[A, ARepr],
    bGen: LabelledGeneric.Aux[B, BRepr],
    inter: hlist.Intersection.Aux[ARepr, BRepr, Unaligned],
    align: hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] =
    a => bGen.from(align.apply(inter.apply(aGen.to(a))))
}

object Main extends App {
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2a])
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2b])
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2c])
}

