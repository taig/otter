package io.taig.otter

// import cats.Order
// import cats.data.Chain
// import cats.data.NonEmptyChain
// import cats.data.NonEmptyList
// import cats.data.NonEmptyMap
// import cats.data.NonEmptySeq
// import cats.data.NonEmptySet
// import cats.data.NonEmptyVector
import cats.implicits.*
// import io.taig.otter as Base

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
// import cats.Eval
// import io.taig.enumeration.ext.Mapping
// import io.taig.enumeration.ext.EnumerationValues
// import cats.kernel.Eq

trait Codecs extends Types:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]] = none,
      maximum: Option[Comparison[JBigDecimal]] = none,
      multiple: Option[JBigDecimal] = none
  ): Primitive[JBigDecimal] =
    Primitive.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty)

  val jBigDecimal: Primitive[JBigDecimal] = jBigDecimal()

  def bigDecimal(
      minimum: Option[Comparison[BigDecimal]] = none,
      maximum: Option[Comparison[BigDecimal]] = none,
      multiple: Option[BigDecimal] = none
  ): Primitive[BigDecimal] = jBigDecimal(
    minimum.map(_.map(_.bigDecimal)),
    maximum.map(_.map(_.bigDecimal)),
    multiple.map(_.bigDecimal)
  ).imap(BigDecimal.apply)(_.bigDecimal)

  val bigDecimal: Primitive[BigDecimal] = bigDecimal()

  def jBigInteger(
      minimum: Option[Comparison[JBigInteger]] = none,
      maximum: Option[Comparison[JBigInteger]] = none,
      multiple: Option[JBigInteger] = none
  ): Primitive[JBigInteger] =
    Primitive.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty)

  val jBigInteger: Primitive[JBigInteger] = jBigInteger()

  def bigInt(
      minimum: Option[Comparison[BigInt]] = none,
      maximum: Option[Comparison[BigInt]] = none,
      multiple: Option[BigInt] = none
  ): Primitive[BigInt] = jBigInteger(
    minimum.map(_.map(_.bigInteger)),
    maximum.map(_.map(_.bigInteger)),
    multiple.map(_.bigInteger)
  ).imap(BigInt.apply)(_.bigInteger)

  val bigInt: Primitive[BigInt] = bigInt()

  val boolean: Primitive[Boolean] = Primitive.Boolean(metadata = Metadata.Empty)

  def double(
      minimum: Option[Comparison[Double]] = none,
      maximum: Option[Comparison[Double]] = none,
      multiple: Option[Double] = none
  ): Primitive[Double] = Primitive.Double(minimum, maximum, multiple, metadata = Metadata.Empty)

  val double: Primitive[Double] = double()

  def float(
      minimum: Option[Comparison[Float]] = none,
      maximum: Option[Comparison[Float]] = none,
      multiple: Option[Float] = none
  ): Primitive[Float] = Primitive.Float(minimum, maximum, multiple, metadata = Metadata.Empty)

  val float: Primitive[Float] = float()

  def int(
      minimum: Option[Comparison[Int]] = none,
      maximum: Option[Comparison[Int]] = none,
      multiple: Option[Int] = none
  ): Primitive[Int] = Primitive.Int(minimum, maximum, multiple, metadata = Metadata.Empty)

  val int: Primitive[Int] = int()

  def long(
      minimum: Option[Comparison[Long]] = none,
      maximum: Option[Comparison[Long]] = none,
      multiple: Option[Long] = none
  ): Primitive[Long] = Primitive.Long(minimum, maximum, multiple, metadata = Metadata.Empty)

  val long: Primitive[Long] = long()

  object string extends StringCodecBuilder[String]:
    override protected def empty: String = ""
    override protected def isEmpty(a: String): Boolean = a.isEmpty

    def apply(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): Primitive[String] = Primitive.String(minimum, maximum, matches, metadata = Metadata.Empty)

  val pattern: Primitive[Pattern] = string.imap(Pattern.compile)(_.pattern)

  def parser[A](
      name: String,
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      matches: Option[Pattern] = none
  )(f: String => Either[String, A])(g: A => String): Primitive[A] =
    Primitive.Parser(name, decode = f, encode = g, minimum, maximum, matches, metadata = Metadata.Empty)

  val uuid: Primitive[UUID] = parser(name = "uuid") { value =>
    Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
  }(_.show)

//   def field[F <: Codec[A], A](name: String, codec: => F): Field.Required.Of[F, A] =
//     Base.Field.Required.Root(name, codec = Eval.later(codec), metadata = Metadata.Empty)

//   def branch[F <: Codec[A], A](name: String, codec: => F): Branch.Of[F, A] =
//     Base.Branch.Root(name, codec = Eval.later(codec), metadata = Metadata.Empty)

//   object collection:
//     def list[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, List[A]] =
//       Base.Collection.Linked(codec = Eval.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)

//     def nonEmptyList[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, NonEmptyList[A]] =
//       list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
//         .imap(NonEmptyList.fromListUnsafe)(_.toList)

//     def vector[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, Vector[A]] =
//       Base.Collection.Indexed(codec = Eval.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)

//     def nonEmptyVector[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, NonEmptyVector[A]] =
//       vector(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
//         .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

//     def seq[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, Seq[A]] = vector(codec, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

//     def nonEmptySeq[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, NonEmptySeq[A]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
//       .imap(values => NonEmptySeq(values.head, values.tail))(values =>
//         NonEmptyVector(values.head, values.tail.toVector)
//       )

//     def chain[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, Chain[A]] =
//       vector(codec, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

//     def nonEmptyChain[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, NonEmptyChain[A]] =
//       nonEmptyVector(codec, minimum, maximum, uniqueItems).imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

//     def set[F <: Codec[A], A](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, Set[A]] = vector(codec, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

//     def sortedSet[F <: Codec[A], A: Order](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, SortedSet[A]] = list(codec, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

//     def nonEmptySet[F <: Codec[A], A: Order](
//         codec: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none,
//         uniqueItems: Boolean = false
//     ): Collection.Of[F, NonEmptySet[A]] = nonEmptyList(codec, minimum, maximum, uniqueItems)
//       .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)

//   object dictionary:
//     def list[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, List[(A, B)]] = Base.Dictionary.Root(
//       key = Eval.later(key),
//       value = Eval.later(value),
//       minimum,
//       maximum,
//       metadata = Metadata.Empty
//     )

//     def nonEmptyList[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.max(1.some), maximum)
//       .imap(NonEmptyList.fromListUnsafe)(_.toList)

//     def vector[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

//     def nonEmptyVector[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.max(1.some), maximum)
//       .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

//     def seq[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

//     def nonEmptySeq[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.max(1.some), maximum)
//       .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

//     def chain[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

//     def nonEmptyChain[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.max(1.some), maximum)
//       .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

//     def map[F <: Codec[B], A, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

//     def sortedMap[F <: Codec[B], A: Order, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

//     def nonEmptyMap[F <: Codec[B], A: Order, B](
//         key: => Primitive[A],
//         value: => F,
//         minimum: Option[Int] = none,
//         maximum: Option[Int] = none
//     ): Dictionary.Of[F, NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.max(1.some), maximum)
//       .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

//   def enumeration[A, B](codec: => Primitive[A])(using
//       mapping: Mapping[B, A]
//   ): Enumeration[B] =
//     Base.Enumeration.Root(codec = Eval.later(codec), mapping, metadata = Metadata.Empty)

//   def enumeration[A: Order, B](codec: => Primitive[A])(f: B => A)(using
//       EnumerationValues.Aux[B, B]
//   ): Enumeration[B] = enumeration(codec)(using Mapping.enumeration(f))

//   object constant:
//     def apply[F <: Codec[A], A: Eq](codec: => F, a: A): Constant.Of[F, A] =
//       Base.Constant.Root(codec = Eval.later(codec), reference = a, metadata = Metadata.Empty)
//     def apply(value: String): Constant.Of[Primitive[?], String] = apply(string, value)
//     def apply(value: Int): Constant.Of[Primitive[?], Int] = apply(int, value)
//     def apply(value: Long): Constant.Of[Primitive[?], Long] = apply(long, value)
//     def apply(value: Float): Constant.Of[Primitive[?], Float] = apply(float, value)
//     def apply(value: Double): Constant.Of[Primitive[?], Double] = apply(double, value)
//     def apply(value: Boolean): Constant.Of[Primitive[?], Boolean] = apply(boolean, value)

//   // object dynamic:
//   //   val number: Union[Data.Number] = branch("bigDecimal", jBigDecimal) |
//   //     branch("bigInteger", jBigInteger) |
//   //     branch("double", double) |
//   //     branch("float", float) |
//   //     branch("int", int) |
//   //     branch("long", long)

//   //   val primitive: Union.Of[Data.Primitive, Data.Primitive] =
//   //     number | branch("boolean", boolean) | branch("string", string)

//   //   val value: Union.Of[Data.Value, Data.Value] = primitive |
//   //     branch("object", dictionary.list(string, any).imap(Data.Object.apply)(_.values)) |
//   //     branch("array", collection.vector(any).imap(Data.Array.apply)(_.values))

//   //   // This code will trigger a warning, which might be wrong
//   //   // val any: Union.Of[Data.Any, Data.Any] = value | branch("null", nil.as(Data.Null))

//   //   val any: Union.Of[Data.Any, Data.Value | Data.Null] = (value :+ branch("null", nil.as(Data.Null))).imap {
//   //     case Left(value)  => value
//   //     case Right(value) => value
//   //   } {
//   //     case value: Data.Value => Left(value)
//   //     case a: Data.Null      => Right(a)
//   //   }

//   // val void: Optional.Of[Data.Any, Unit] = Base.Optional.Void(metadata = Metadata.Empty)

//   // val nil: Optional.Of[Data.Null, Unit] = Base.Optional.Null(metadata = Metadata.Empty)

// //   val xpath: Primitive[XPath] = parser(name = "xpath")(XPath.parse(_).toOption)(_.show)

// object Codecs extends Codecs
