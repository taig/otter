package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import io.taig.otter as Base

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import cats.Eval
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues

trait Codecs extends Types:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]] = none,
      maximum: Option[Comparison[JBigDecimal]] = none,
      multiple: Option[JBigDecimal] = none
  ): Primitive.Of[Format.Number, JBigDecimal] =
    Base.Primitive.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty)

  val jBigDecimal: Primitive.Of[Format.Number, JBigDecimal] = jBigDecimal()

  def bigDecimal(
      minimum: Option[Comparison[BigDecimal]] = none,
      maximum: Option[Comparison[BigDecimal]] = none,
      multiple: Option[BigDecimal] = none
  ): Primitive.Of[Format.Number, BigDecimal] = jBigDecimal(
    minimum.map(_.map(_.bigDecimal)),
    maximum.map(_.map(_.bigDecimal)),
    multiple.map(_.bigDecimal)
  ).imap(BigDecimal.apply)(_.bigDecimal)

  val bigDecimal: Primitive.Of[Format.Number, BigDecimal] = bigDecimal()

  def jBigInteger(
      minimum: Option[Comparison[JBigInteger]] = none,
      maximum: Option[Comparison[JBigInteger]] = none,
      multiple: Option[JBigInteger] = none
  ): Primitive.Of[Format.Number, JBigInteger] =
    Base.Primitive.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty)

  val jBigInteger: Primitive.Of[Format.Number, JBigInteger] = jBigInteger()

  def bigInt(
      minimum: Option[Comparison[BigInt]] = none,
      maximum: Option[Comparison[BigInt]] = none,
      multiple: Option[BigInt] = none
  ): Primitive.Of[Format.Number, BigInt] = jBigInteger(
    minimum.map(_.map(_.bigInteger)),
    maximum.map(_.map(_.bigInteger)),
    multiple.map(_.bigInteger)
  ).imap(BigInt.apply)(_.bigInteger)

  val bigInt: Primitive.Of[Format.Number, BigInt] = bigInt()

  val boolean: Primitive.Of[Format.Boolean, Boolean] = Base.Primitive.Boolean(metadata = Metadata.Empty)

  def double(
      minimum: Option[Comparison[Double]] = none,
      maximum: Option[Comparison[Double]] = none,
      multiple: Option[Double] = none
  ): Primitive.Of[Format.Number, Double] = Base.Primitive.Double(minimum, maximum, multiple, metadata = Metadata.Empty)

  val double: Primitive.Of[Format.Number, Double] = double()

  def float(
      minimum: Option[Comparison[Float]] = none,
      maximum: Option[Comparison[Float]] = none,
      multiple: Option[Float] = none
  ): Primitive.Of[Format.Number, Float] = Base.Primitive.Float(minimum, maximum, multiple, metadata = Metadata.Empty)

  val float: Primitive.Of[Format.Number, Float] = float()

  def int(
      minimum: Option[Comparison[Int]] = none,
      maximum: Option[Comparison[Int]] = none,
      multiple: Option[Int] = none
  ): Primitive.Of[Format.Number, Int] = Base.Primitive.Int(minimum, maximum, multiple, metadata = Metadata.Empty)

  val int: Primitive.Of[Format.Number, Int] = int()

  def long(
      minimum: Option[Comparison[Long]] = none,
      maximum: Option[Comparison[Long]] = none,
      multiple: Option[Long] = none
  ): Primitive.Of[Format.Number, Long] = Base.Primitive.Long(minimum, maximum, multiple, metadata = Metadata.Empty)

  val long: Primitive.Of[Format.Number, Long] = long()

  object string extends StringCodecBuilder[String]:
    override protected def empty: String = ""
    override protected def isEmpty(a: String): Boolean = a.isEmpty

    def apply(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): Primitive.Of[Format.String, String] = Base.Primitive.String(minimum, maximum, matches, metadata = Metadata.Empty)

  val pattern: Primitive.Of[Format.String, Pattern] = string().imap(Pattern.compile)(_.pattern)

  def parser[A](
      name: String,
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      matches: Option[Pattern] = none
  )(f: String => Either[String, A])(g: A => String): Primitive.Of[Format.String, A] =
    Base.Primitive.Parser(name, decode = f, encode = g, minimum, maximum, matches, metadata = Metadata.Empty)

  val uuid: Primitive.Of[Format.String, UUID] = parser(name = "uuid") { value =>
    Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
  }(_.show)

  def field[A <: Format.Any, B](name: String, codec: Codec.Of[A, B]): Field.Required.Of[A, B] =
    Base.Field.Required(name, codec, metadata = Metadata.Empty)

  object branch:
    def apply[F <: Format.Any, A](name: String, codec: => Codec.Of[F, A]): Branch.Of[F, A] =
      Base.Branch.Root(name, codec = Eval.later(codec), metadata = Metadata.Empty)

//     def nested[O <: Data.Value, A](
//         name: String,
//         codec: => Codec.Of[Data.Nullable[O], A],
//         discriminator: Discriminator.Nested = Discriminator.Nested.Default
//     ): Branch.Nested.Of[O, A] =
//       val record: Record.Of[Data.String | O, A] =
//         field(discriminator.identifier, constant(name)) :* field.optional(discriminator.value, codec)
//       Base.Branch.Tagged.Apply(name, codec = Eval.now(record), discriminator)

//     def merged[O <: Data, A](
//         name: String,
//         codec: => Record.Of[O, A],
//         discriminator: Discriminator.Merged = Discriminator.Merged.Default
//     ): Branch.Merged.Of[O, A] =
//       val record = field(discriminator.identifier, constant(name)) *: codec
//       Base.Branch.Tagged.Apply(name, codec = Eval.now(record), discriminator)

//     def keyed[O <: Data.Value, A](name: String, codec: => Codec.Of[Data.Nullable[O], A]): Branch.Keyed.Of[O, A] =
//       val record = field.optional(name, codec).toRecord
//       Base.Branch.Tagged.Apply(name, codec = Eval.now(record), Discriminator.Keyed)

  object collection:
    def list[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, List[B]] =
      Base.Collection.Linked(codec = Eval.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)

    def nonEmptyList[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, NonEmptyList[B]] =
      list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
        .imap(NonEmptyList.fromListUnsafe)(_.toList)

    def vector[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, Vector[B]] =
      Base.Collection.Indexed(codec = Eval.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)

    def nonEmptyVector[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, NonEmptyVector[B]] =
      vector(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
        .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    def seq[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, Seq[B]] = vector(codec, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

    def nonEmptySeq[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, NonEmptySeq[B]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    def chain[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, Chain[B]] =
      vector(codec, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, NonEmptyChain[B]] =
      nonEmptyVector(codec, minimum, maximum, uniqueItems).imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    def set[A <: Format.Any, B](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, Set[B]] = vector(codec, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

    def sortedSet[A <: Format.Any, B: Order](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, SortedSet[B]] = list(codec, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

    def nonEmptySet[A <: Format.Any, B: Order](
        codec: => Codec.Of[A, B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[A, NonEmptySet[B]] = nonEmptyList(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)

  object dictionary:
    def list[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, List[(A, C)]] = Base.Dictionary.Root(key, value, minimum, maximum, metadata = Metadata.Empty)

    def nonEmptyList[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, NonEmptyList[(A, C)]] = list(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    def vector[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, Vector[(A, C)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    def nonEmptyVector[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, NonEmptyVector[(A, C)]] = vector(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    def seq[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, Seq[(A, C)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    def nonEmptySeq[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, NonEmptySeq[(A, C)]] = seq(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    def chain[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, Chain[(A, C)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    def nonEmptyChain[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, NonEmptyChain[(A, C)]] = chain(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    def map[A, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, Map[A, C]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    def sortedMap[A: Order, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, SortedMap[A, C]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    def nonEmptyMap[A: Order, B <: Format.Any, C](
        key: Codec.Of[Format.Primitive, A],
        value: Codec.Of[B, C],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Dictionary.Of[B, NonEmptyMap[A, C]] = sortedMap(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

  def enumeration[F <: Format.Primitive, A, B](codec: => Codec.Of[F, A])(using
      mapping: Mapping[B, A]
  ): Enumeration.Of[F, B] =
    Base.Enumeration.Root(codec = Eval.later(codec), mapping, metadata = Metadata.Empty)

  def enumeration[F <: Format.Primitive, A: Order, B](codec: => Codec.Of[F, A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration.Of[F, B] = enumeration(codec)(using Mapping.enumeration(f))

  object constant:
    def apply[A <: Format.Primitive, B](codec: => Codec.Of[A, B], b: B): Constant.Of[A, Unit] =
      Base.Constant.Root(codec = Eval.later(codec), value = b, metadata = Metadata.Empty)
    def apply(value: String): Constant.Of[Format.String, Unit] = apply(string, value)
    def apply(value: Int): Constant.Of[Format.Number, Unit] = apply(int, value)
    def apply(value: Long): Constant.Of[Format.Number, Unit] = apply(long, value)
    def apply(value: Float): Constant.Of[Format.Number, Unit] = apply(float, value)
    def apply(value: Double): Constant.Of[Format.Number, Unit] = apply(double, value)
    def apply(value: Boolean): Constant.Of[Format.Boolean, Unit] = apply(boolean, value)

  object dynamic:
    val number: Union.Of[Format.Number, Data.Number] = branch("bigDecimal", jBigDecimal) |
      branch("bigInteger", jBigInteger) |
      branch("double", double) |
      branch("float", float) |
      branch("int", int) |
      branch("long", long)

    val primitive: Union.Of[Format.Primitive, Data.Primitive] =
      number | branch("boolean", boolean) | branch("string", string)

    val value: Union.Of[Format.Value, Data.Value] = primitive |
      branch("object", dictionary.list(string, any).imap(Data.Object.apply)(_.values)) |
      branch("array", collection.list(any).imap(Data.Array.apply)(_.values))

    // val any: Union.Of[Format.Any, Data.Any] = value | branch("null", nil.as(Data.Null))

    val any: Union.Of[Format.Any, Data.Value | Data.Null] = (value :+ branch("null", nil.as(Data.Null))).imap {
      case Left(value) => value
      case Right(value)    => value
    } {
      case value: Data.Value => Left(value)
      case a: Data.Null         => Right(a)
    }

    // val any: Union.Of[Format.Any, Data.Any] = value | branch("null", nil)

  val void: Optional.Of[Format.Any, Unit] = Base.Optional.Void(metadata = Metadata.Empty)

  val nil: Optional.Of[Format.Null, Unit] = Base.Optional.Null(metadata = Metadata.Empty)

//   val void: Dynamic.Of[Data.Null, Unit] = dynamic.nil.const(Data.Null)

//   val empty: Record.Of[Nothing, Unit] = Base.Record.Empty

//   def singleton[A](a: A): Dynamic.Of[Data.Null, a.type] = void.as(a)

//   val xpath: Primitive.Of[Data.String, XPath] = parser(name = "xpath")(XPath.parse(_).toOption)(_.show)

object Codecs extends Codecs
