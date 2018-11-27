package ch6

import shapeless._
import shapeless.ops.hlist
import cats._
import cats.implicits._
import shapeless.labelled._
import Ch6._

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
  A, B, ARepr <: HList, BRepr <: HList,
  Common <: HList, Added <: HList, Unaligned <: HList
  ](
    implicit
    aGen: LabelledGeneric.Aux[A, ARepr],
    bGen: LabelledGeneric.Aux[B, BRepr],
    inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
    diff: hlist.Diff.Aux[BRepr, Common, Added],
    monoid: Monoid[Added],
    prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
    align: hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] =
    a => bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))

  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty: A = zero
      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val hnilMonoid: Monoid[HNil] =
    createMonoid[HNil](HNil)((_, _) => HNil)

  implicit def hlistMonoid[K <: Symbol, H, T <: HList] (
    implicit
    hMonoid: Lazy[Monoid[H]],
    tMonoid: Monoid[T]
  ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
      (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) ::
          tMonoid.combine(x.tail, y.tail)
    }
}

object Main extends App {
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2a])
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2b])
  println(IceCreamV1("Sundae", 1, inCone = true).migrateTo[IceCreamV2c])
}

