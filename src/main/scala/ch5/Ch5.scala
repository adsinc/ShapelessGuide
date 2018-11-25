package ch5

import ch2.Ch2.IceCream
import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness, ::}
import shapeless.labelled.FieldType
import shapeless.syntax.singleton._
import ch5.Ch5._

object Ch5 {
  val numCherries = "numCherries" ->> 123

  def getFieldName[K, V](value: FieldType[K, V])
    (implicit witness: Witness.Aux[K]): K =
    witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  sealed trait JsonValue
  final case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  final case class JsonArray(items: List[JsonValue]) extends JsonValue
  final case class JsonString(value: String) extends JsonValue
  final case class JsonNumber(value: Double) extends JsonValue
  final case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
  }

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    value => func(value)

  implicit val stringEncoder: JsonEncoder[String] =
    JsonString.apply

  implicit val doubleEncoder: JsonEncoder[Double] =
    JsonNumber.apply

  implicit val intEncoder: JsonEncoder[Int] =
    num => JsonNumber(num)

  implicit val booleanEncode: JsonEncoder[Boolean] =
    JsonBoolean.apply

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    list => JsonArray(list.map(enc.encode))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    opt => opt.map(enc.encode).getOrElse(JsonNull)

  val iceCream = IceCream("Sundae", 1, inCone = false)

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    _ => JsonObject(Nil)

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    hlist => {
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    value => hEncoder.value.encode(generic.to(value))
}

object Main extends App {
  println(getFieldName(numCherries))
  println(getFieldValue(numCherries))

  println(JsonEncoder[IceCream].encode(iceCream))
}