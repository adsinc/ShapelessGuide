package ch3
import ch2.Ch2.{Circle, IceCream, Rectangle, Shape}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

object Ch3 {

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  implicit val employeeEncoder: CsvEncoder[Employee] =
    e => List(
      e.name,
      e.number.toString,
      if (e.manager) "yes" else "no"
    )

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(","))
      .mkString("\n")

  val employees: List[Employee] = List(
    Employee("Bill", 1, manager = true),
    Employee("Peter", 2, manager = false),
    Employee("Milton", 3, manager = false)
  )

  val iceCreams = List(
    IceCream("Sundae", 1, inCone = false),
    IceCream("Cornetto", 0, inCone = true),
    IceCream("Banana Split", 0, inCone = false),
  )

  implicit def pairEncoder[A, B](
    implicit
    aEncoder: CsvEncoder[A],
    bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    p => aEncoder.encode(p._1) ++ bEncoder.encode(p._2)

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    value => func(value)

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    b => if(b) List("yes") else List("no")

  implicit val stringEncoder: CsvEncoder[String] =
    List(_)

  implicit val intEncoder: CsvEncoder[Int] =
    num => List(num.toString)

  implicit val hnilEncoder: CsvEncoder[HNil] =
    _ => Nil

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] =
    a => enc.value.encode(gen.to(a))

  implicit val cnilEncoder: CsvEncoder[CNil] =
    _ => throw new Exception("Inconceivable!")

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(h) => tEncoder.encode(h)
  }

  implicit val doubleEncoder: CsvEncoder[Double] =
    d => List(d.toString)

  val shapes: List[Shape] = List(
    Rectangle(3, 4),
    Circle(1)
  )

  sealed trait Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
}

object Main extends App {
  import ch3.Ch3._

  println(writeCsv(employees))

  println(writeCsv(iceCreams))

//  println(writeCsv(employees zip iceCreams))

  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))

  println(writeCsv(shapes))

  CsvEncoder[Tree[Int]]
}



