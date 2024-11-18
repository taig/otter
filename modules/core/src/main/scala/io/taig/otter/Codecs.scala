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
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter as Base

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.annotation.targetName
import scala.collection.immutable.Map
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

trait Codecs extends Types:
  self =>

  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]] = none,
      maximum: Option[Comparison[JBigDecimal]] = none,
      multiple: Option[JBigDecimal] = none
  ): Primitive.Of[Data.Number, JBigDecimal] = Base.Primitive.jBigDecimal(minimum, maximum, multiple)

  val jBigDecimal: Primitive.Of[Data.Number, JBigDecimal] = jBigDecimal()

  def bigDecimal(
      minimum: Option[Comparison[BigDecimal]] = none,
      maximum: Option[Comparison[BigDecimal]] = none,
      multiple: Option[BigDecimal] = none
  ): Primitive.Of[Data.Number, BigDecimal] = jBigDecimal(
    minimum.map(_.map(_.bigDecimal)),
    maximum.map(_.map(_.bigDecimal)),
    multiple.map(_.bigDecimal)
  ).imap(BigDecimal.apply)(_.bigDecimal)

  val bigDecimal: Primitive.Of[Data.Number, BigDecimal] = bigDecimal()

  def jBigInteger(
      minimum: Option[Comparison[JBigInteger]] = none,
      maximum: Option[Comparison[JBigInteger]] = none,
      multiple: Option[JBigInteger] = none
  ): Primitive.Of[Data.Number, JBigInteger] = Base.Primitive.jBigInteger(minimum, maximum, multiple)

  val jBigInteger: Primitive.Of[Data.Number, JBigInteger] = jBigInteger()

  def bigInt(
      minimum: Option[Comparison[BigInt]] = none,
      maximum: Option[Comparison[BigInt]] = none,
      multiple: Option[BigInt] = none
  ): Primitive.Of[Data.Number, BigInt] = jBigInteger(
    minimum.map(_.map(_.bigInteger)),
    maximum.map(_.map(_.bigInteger)),
    multiple.map(_.bigInteger)
  ).imap(BigInt.apply)(_.bigInteger)

  val bigInt: Primitive.Of[Data.Number, BigInt] = bigInt()

  def double(
      minimum: Option[Comparison[Double]] = none,
      maximum: Option[Comparison[Double]] = none,
      multiple: Option[Double] = none
  ): Primitive.Of[Data.Number, Double] = Base.Primitive.double(minimum, maximum, multiple)

  val double: Primitive.Of[Data.Number, Double] = double()

  def float(
      minimum: Option[Comparison[Float]] = none,
      maximum: Option[Comparison[Float]] = none,
      multiple: Option[Float] = none
  ): Primitive.Of[Data.Number, Float] = Base.Primitive.float(minimum, maximum, multiple)

  val float: Primitive.Of[Data.Number, Float] = float()

  def int(
      minimum: Option[Comparison[Int]] = none,
      maximum: Option[Comparison[Int]] = none,
      multiple: Option[Int] = none
  ): Primitive.Of[Data.Number, Int] = Base.Primitive.int(minimum, maximum, multiple)

  val int: Primitive.Of[Data.Number, Int] = int()

  def long(
      minimum: Option[Comparison[Long]] = none,
      maximum: Option[Comparison[Long]] = none,
      multiple: Option[Long] = none
  ): Primitive.Of[Data.Number, Long] = Base.Primitive.long(minimum, maximum, multiple)

  val long: Primitive.Of[Data.Number, Long] = long()

  val boolean: Primitive.Of[Data.Boolean, Boolean] = Base.Primitive.boolean

  abstract class StringCodecBuilder[A]:
    protected def apply(
        minLength: Option[Int],
        maxLength: Option[Int],
        matches: Option[Pattern]
    ): Primitive.Of[Data.String, A]

    protected def isEmpty(a: A): Boolean
    protected def empty: A

    final def apply(minLength: Int, maxLength: Int): Primitive.Of[Data.String, A] =
      apply(minLength = minLength.some, maxLength = maxLength.some, matches = none)
    final def matches(
        pattern: String,
        minLength: Option[Int] = none,
        maxLength: Option[Int] = none
    ): Primitive.Of[Data.String, A] =
      apply(minLength = none, maxLength = none, matches = Pattern.compile(Pattern.quote(pattern)).some)
    final def required(maxLength: Option[Int] = none, matches: Option[Pattern] = none): Primitive.Of[Data.String, A] =
      apply(minLength = 1.some, maxLength, matches)
    final def required(maxLength: Int, matches: Pattern): Primitive.Of[Data.String, A] =
      required(maxLength = maxLength.some, matches = matches.some)
    final def required(maxLength: Int): Primitive.Of[Data.String, A] =
      required(maxLength = maxLength.some, matches = none)
    final def required(matches: Pattern): Primitive.Of[Data.String, A] =
      required(maxLength = none, matches = matches.some)
    final val required: Primitive.Of[Data.String, A] = required()
    final val nonEmpty: Primitive.Of[Data.String, Option[A]] =
      apply(minLength = none, maxLength = none, matches = none).imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))

  final def string(
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  ): Primitive.Of[Data.String, String] = Base.Primitive.string(minLength, maxLength, matches)

  final val string: Primitive.Of[Data.String, String] = string()

  given [A]: Conversion[string.type, StringCodecBuilder[String]] = _ =>
    new StringCodecBuilder[String]:
      override def apply(
          minLength: Option[Int],
          maxLength: Option[Int],
          matches: Option[Pattern]
      ): Primitive.Of[Data.String, String] = string(minLength, maxLength, matches)
      override def isEmpty(a: String): Boolean = a.isEmpty
      override val empty: String = ""

  val pattern: Primitive.Of[Data.String, Pattern] = string.imap(Pattern.compile)(_.pattern)

  def parser[A](
      name: String,
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  )(f: String => Option[A])(g: A => String): Primitive.Of[Data.String, A] =
    Base.Primitive.parser(name, minLength, maxLength, matches, f, g)

  val uuid: Primitive.Of[Data.String, UUID] = parser(name = "uuid")(value =>
    try UUID.fromString(value).some
    catch { case _: java.lang.IllegalArgumentException => none }
  )(_.show)

  object field:
    def apply[O <: Data.Value, A](name: String, codec: Codec.Of[Data.Nullable[O], A]): Field.Of[O, A] =
      optional(name, codec)

    @targetName("required")
    def apply[O <: Data.Value, A](name: String, codec: Codec.Of[O, A]): Field.Required.Of[O, A] =
      Base.Field.Required(name, codec, metadata = Metadata.Empty)

    def nullable[O <: Data, A](name: String, codec: Codec.Of[O, A]): Field.Of[O, A] =
      Base.Field.Nullable(name, codec, metadata = Metadata.Empty)

    def optional[O <: Data.Value, A](name: String, codec: Codec.Of[Data.Nullable[O], A]): Field.Of[O, A] =
      Base.Field.Optional(name, codec, metadata = Metadata.Empty)

  object branch:
    def apply[O <: Data, A](name: String, codec: Codec.Of[O, A]): Branch.Of[O, A] = Base.Branch(name, codec)

    def nested[O <: Data.Value, A](
        name: String,
        codec: Codec.Of[Data.Nullable[O], A],
        discriminator: Discriminator.Nested = Discriminator.Nested.Default
    ): Branch.Nested.Of[O, A] =
      val record: Record.Of[Data.String | O, A] =
        field(discriminator.identifier, constant(name)) :* field.optional(discriminator.value, codec)
      Base.Branch.Tagged(name, record, discriminator)

    def merged[O <: Data, A](
        name: String,
        codec: Record.Of[O, A],
        discriminator: Discriminator.Merged = Discriminator.Merged.Default
    ): Branch.Merged.Of[O, A] =
      val record = field(discriminator.identifier, constant(name)) *: codec
      Base.Branch.Tagged(name, record, discriminator)

    def keyed[O <: Data.Value, A](name: String, codec: Codec.Of[Data.Nullable[O], A]): Branch.Keyed.Of[O, A] =
      val record = field.optional(name, codec).toRecord
      Base.Branch.Tagged(name, record, Discriminator.Keyed)

  object collection:
    def vector[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, Vector[A]] = Base.Collection(codec, minItems, maxItems, uniqueItems)

    def nonEmptyVector[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, NonEmptyVector[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap(NonEmptyVector.apply)(fa => (fa.head, fa.tail))

    def seq[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, Seq[A]] = vector(codec, minItems, maxItems, uniqueItems).imap(identity)(_.toVector)

    def nonEmptySeq[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, NonEmptySeq[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail.toVector))

    def list[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, List[A]] = vector(codec, minItems, maxItems, uniqueItems).imap(_.toList)(_.toVector)

    def nonEmptyList[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, NonEmptyList[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap { case (head, tail) => NonEmptyList(head, tail.toList) }(fa => (fa.head, fa.tail.toVector))

    def chain[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, Chain[A]] =
      vector(codec, minItems, maxItems, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Of[O, NonEmptyChain[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap { case (head, tail) => NonEmptyChain(head, tail*) }(fa => (fa.head, fa.tail.toVector))

    def set[O <: Data, A](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Of[O, Set[A]] =
      vector(codec, minItems, maxItems, uniqueItems = true).imap(_.toSet)(_.toVector)

    def sortedSet[O <: Data, A: Order](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Of[O, SortedSet[A]] =
      vector(codec, minItems, maxItems, uniqueItems = true).imap(SortedSet.from)(_.toVector)

    def nonEmptySet[O <: Data, A: Order](
        codec: Codec.Of[O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Of[O, NonEmptySet[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems = true)
      .imap { case (head, tail) => NonEmptySet(head, SortedSet.from(tail)) }(fa => (fa.head, fa.tail.toVector))

  object dictionary:
    def vector[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, Vector[(A, B)]] = Base.Dictionary(key, value, minProperties, maxProperties)

    def nonEmptyVector[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, NonEmptyVector[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap(NonEmptyVector.apply)(fa => (fa.head, fa.tail))

    def seq[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, Seq[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(identity)(_.toVector)

    def nonEmptySeq[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, NonEmptySeq[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail.toVector))

    def list[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, List[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(_.toList)(_.toVector)

    def nonEmptyList[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, NonEmptyList[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyList(head, tail.toList) }(fa => (fa.head, fa.tail.toVector))

    def chain[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, Chain[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, NonEmptyChain[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyChain(head, tail*) }(fa => (fa.head, fa.tail.toVector))

    def map[O <: Data, A, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, Map[A, B]] =
      vector(key, value, minProperties, maxProperties).imap(_.to(Map))(_.toVector)

    def sortedMap[O <: Data, A: Order, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, SortedMap[A, B]] =
      vector(key, value, minProperties, maxProperties).imap(SortedMap.from)(_.toVector)

    def nonEmptyMap[O <: Data, A: Order, B](
        key: Codec.Of[Data.Primitive, A],
        value: Codec.Of[O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Of[O, NonEmptyMap[A, B]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyMap(head, SortedMap.from(tail)) }(fa => (fa.head, fa.tail.toVector))

  def enumeration[A, B](codec: Codec.Of[Data.Primitive, A])(using
      mapping: Mapping[B, A]
  ): Enumeration[B] = Base.Enumeration(codec, mapping)

  def enumeration[A: Order, B](codec: Codec.Of[Data.Primitive, A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration[B] = enumeration(codec)(using Mapping.enumeration(f))

  object constant:
    def apply[O <: Data.Primitive, A](codec: Codec.Of[O, A], a: A): Constant.Of[O, Unit] = Base.Constant(codec, a)
    def apply(value: String): Constant.Of[Data.String, Unit] = apply(string, value)
    def apply(value: Int): Constant.Of[Data.Number, Unit] = apply(int, value)
    def apply(value: Long): Constant.Of[Data.Number, Unit] = apply(long, value)
    def apply(value: Float): Constant.Of[Data.Number, Unit] = apply(float, value)
    def apply(value: Double): Constant.Of[Data.Number, Unit] = apply(double, value)
    def apply(value: Boolean): Constant.Of[Data.Boolean, Unit] = apply(boolean, value)

  object dynamic:
    val any: Dynamic.Of[Data, Data] = Base.Dynamic.Any
    val value: Dynamic.Of[Data.Value, Data.Value] = Base.Dynamic.Value
    val obj: Dynamic.Of[Data.Object[?], Data.Object[?]] = Base.Dynamic.Object
    val array: Dynamic.Of[Data.Array[?], Data.Array[?]] = Base.Dynamic.Array
    val primitive: Dynamic.Of[Data.Primitive, Data.Primitive] = Base.Dynamic.Primitive
    val number: Dynamic.Of[Data.Number, Data.Number] = Base.Dynamic.Number
    val nil: Dynamic.Of[Data.Null, Data.Null] = Base.Dynamic.Null

  val void: Dynamic.Of[Data.Null, Unit] = dynamic.nil.const(Data.Null)

  def singleton[A](a: A): Dynamic.Of[Data.Null, a.type] = void.as(a)

  val xpath: Primitive.Of[Data.String, XPath] = parser(name = "xpath")(XPath.parse(_).toOption)(_.show)

object Codecs extends Codecs
