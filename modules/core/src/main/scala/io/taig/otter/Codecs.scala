package io.taig.otter

import cats.data.NonEmptySeq
import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.data.NonEmptyMap
import io.taig.otter as Base
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.data.Chain
import cats.Order
import cats.implicits.*
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import scala.collection.immutable.Map
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues
import java.util.regex.Pattern
import java.util.UUID

trait Codecs extends Types:
  self =>

  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]] = none,
      maximum: Option[Comparison[JBigDecimal]] = none,
      multiple: Option[JBigDecimal] = none
  ): Primitive.Required[JBigDecimal] = Base.Primitive.jBigDecimal(minimum, maximum, multiple)

  val jBigDecimal: Primitive.Required[JBigDecimal] = jBigDecimal()

  def bigDecimal(
      minimum: Option[Comparison[BigDecimal]] = none,
      maximum: Option[Comparison[BigDecimal]] = none,
      multiple: Option[BigDecimal] = none
  ): Primitive.Required[BigDecimal] = jBigDecimal(
    minimum.map(_.map(_.bigDecimal)),
    maximum.map(_.map(_.bigDecimal)),
    multiple.map(_.bigDecimal)
  ).imap(BigDecimal.apply)(_.bigDecimal)

  val bigDecimal: Primitive.Required[BigDecimal] = bigDecimal()

  def jBigInteger(
      minimum: Option[Comparison[JBigInteger]] = none,
      maximum: Option[Comparison[JBigInteger]] = none,
      multiple: Option[JBigInteger] = none
  ): Primitive.Required[JBigInteger] = Base.Primitive.jBigInteger(minimum, maximum, multiple)

  val jBigInteger: Primitive.Required[JBigInteger] = jBigInteger()

  def bigInt(
      minimum: Option[Comparison[BigInt]] = none,
      maximum: Option[Comparison[BigInt]] = none,
      multiple: Option[BigInt] = none
  ): Primitive.Required[BigInt] = jBigInteger(
    minimum.map(_.map(_.bigInteger)),
    maximum.map(_.map(_.bigInteger)),
    multiple.map(_.bigInteger)
  ).imap(BigInt.apply)(_.bigInteger)

  val bigInt: Primitive.Required[BigInt] = bigInt()

  def double(
      minimum: Option[Comparison[Double]] = none,
      maximum: Option[Comparison[Double]] = none,
      multiple: Option[Double] = none
  ): Primitive.Required[Double] = Base.Primitive.double(minimum, maximum, multiple)

  val double: Primitive.Required[Double] = double()

  def float(
      minimum: Option[Comparison[Float]] = none,
      maximum: Option[Comparison[Float]] = none,
      multiple: Option[Float] = none
  ): Primitive.Required[Float] = Base.Primitive.float(minimum, maximum, multiple)

  val float: Primitive.Required[Float] = float()

  def int(
      minimum: Option[Comparison[Int]] = none,
      maximum: Option[Comparison[Int]] = none,
      multiple: Option[Int] = none
  ): Primitive.Required[Int] = Base.Primitive.int(minimum, maximum, multiple)

  val int: Primitive.Required[Int] = int()

  def long(
      minimum: Option[Comparison[Long]] = none,
      maximum: Option[Comparison[Long]] = none,
      multiple: Option[Long] = none
  ): Primitive.Required[Long] = Base.Primitive.long(minimum, maximum, multiple)

  val long: Primitive.Required[Long] = long()

  val boolean: Primitive.Required[Boolean] = Base.Primitive.boolean

  def string(
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  ): Primitive.Required[String] = Base.Primitive.string(minLength, maxLength, matches)
  def string(matches: String): Primitive.Required[String] =
    string(matches = Pattern.compile(Pattern.quote(matches)).some)
  val string: Primitive.Required[String] = string()
  val emptyString: Primitive.Required[Option[String]] = string.imap(_.some.filter(_.nonEmpty))(_.orEmpty)

  val pattern: Primitive.Required[Pattern] = string.imap(Pattern.compile)(_.pattern)

  def parser[A](
      name: String,
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  )(
      f: String => Option[A]
  )(g: A => String): Primitive.Required[A] = Base.Primitive.parser(name, minLength, maxLength, matches, f, g)

  val uuid: Primitive.Required[UUID] = parser(name = "uuid")(value =>
    try UUID.fromString(value).some
    catch { case _: java.lang.IllegalArgumentException => none }
  )(_.show)

  def branch[F[+a] <: Data.Optional[a], O <: Data, A](name: String, codec: Base.Codec[F, O, A]): Branch.Of[F[O], A] =
    Base.Branch(name, codec)

  def field[F[+a] <: Data.Optional[a], O <: Data, A](name: String, codec: Base.Codec[F, O, A]): Field.Of[F[O], A] =
    Base.Field(name, codec)

  def record[O <: Data, A](fields: Fields[O, A]): Record.Required.Of[O, A] = fields.toRecord
  def record[O <: Data, A](field: Field.Of[O, A]): Record.Required.Of[O, A] = record(field.toFields)

  def tuple[O <: Data, A](fields: Fields[O, A]): Tuple.Required.Of[O, A] = fields.toTuple
  def tuple[O <: Data, A](field: Field.Of[O, A]): Tuple.Required.Of[O, A] = tuple(field.toFields)

  object sum:
    def nested[O <: Data, A](branches: Branches[O, A]): Sum.Nested.Required.Of[O, A] = branches.toSumNested
    def nested[O <: Data, A](branch: Branch.Of[O, A]): Sum.Nested.Required.Of[O, A] = nested(branch.toBranches)

    def merged[O <: Data, A](branches: Branches[Data.Object[O], A]): Sum.Merged.Required.Of[O, A] =
      branches.toSumMerged
    def merged[O <: Data, A](branch: Branch.Of[Data.Object[O], A]): Sum.Merged.Required.Of[O, A] =
      merged(branch.toBranches)

    def keyed[O <: Data, A](branches: Branches[O, A]): Sum.Keyed.Required.Of[O, A] = branches.toSumKeyed
    def keyed[O <: Data, A](branch: Branch.Of[O, A]): Sum.Keyed.Required.Of[O, A] = keyed(branch.toBranches)

    def untagged[O <: Data, A](branches: Branches[O, A]): Sum.Untagged.Required.Of[O, A] = branches.toSumUntagged
    def untagged[O <: Data, A](branch: Branch.Of[O, A]): Sum.Untagged.Required.Of[O, A] = untagged(branch.toBranches)

  object collection:
    def vector[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], Vector[A]] = Base.Collection(codec, minItems, maxItems, uniqueItems)

    def nonEmptyVector[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], NonEmptyVector[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap(NonEmptyVector.apply)(fa => (fa.head, fa.tail))

    def seq[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], Seq[A]] = vector(codec, minItems, maxItems, uniqueItems).imap(identity)(_.toVector)

    def nonEmptySeq[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], NonEmptySeq[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail.toVector))

    def list[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], List[A]] = vector(codec, minItems, maxItems, uniqueItems).imap(_.toList)(_.toVector)

    def nonEmptyList[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], NonEmptyList[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap { case (head, tail) => NonEmptyList(head, tail.toList) }(fa => (fa.head, fa.tail.toVector))

    def chain[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], Chain[A]] =
      vector(codec, minItems, maxItems, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Collection.Required.Of[F[O], NonEmptyChain[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems)
      .imap { case (head, tail) => NonEmptyChain(head, tail*) }(fa => (fa.head, fa.tail.toVector))

    def set[F[+a] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Required.Of[F[O], Set[A]] =
      vector(codec, minItems, maxItems, uniqueItems = true).imap(_.toSet)(_.toVector)

    def sortedSet[F[+a] <: Data.Optional[a], O <: Data, A: Order](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Required.Of[F[O], SortedSet[A]] =
      vector(codec, minItems, maxItems, uniqueItems = true).imap(SortedSet.from)(_.toVector)

    def nonEmptySet[F[+a] <: Data.Optional[a], O <: Data, A: Order](
        codec: Base.Codec[F, O, A],
        minItems: Option[Int] = none,
        maxItems: Option[Int] = none
    ): Collection.Required.Of[F[O], NonEmptySet[A]] = Base.Collection
      .nonEmpty(codec, minItems, maxItems, uniqueItems = true)
      .imap { case (head, tail) => NonEmptySet(head, SortedSet.from(tail)) }(fa => (fa.head, fa.tail.toVector))

  object dictionary:
    def vector[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], Vector[(A, B)]] = Base.Dictionary(key, value, minProperties, maxProperties)

    def nonEmptyVector[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], NonEmptyVector[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap(NonEmptyVector.apply)(fa => (fa.head, fa.tail))

    def seq[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], Seq[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(identity)(_.toVector)

    def nonEmptySeq[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], NonEmptySeq[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail.toVector))

    def list[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], List[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(_.toList)(_.toVector)

    def nonEmptyList[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], NonEmptyList[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyList(head, tail.toList) }(fa => (fa.head, fa.tail.toVector))

    def chain[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], Chain[(A, B)]] =
      vector(key, value, minProperties, maxProperties).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], NonEmptyChain[(A, B)]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyChain(head, tail*) }(fa => (fa.head, fa.tail.toVector))

    def map[F[+a] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], Map[A, B]] =
      vector(key, value, minProperties, maxProperties).imap(_.to(Map))(_.toVector)

    def sortedMap[F[+a] <: Data.Optional[a], O <: Data, A: Order, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], SortedMap[A, B]] =
      vector(key, value, minProperties, maxProperties).imap(SortedMap.from)(_.toVector)

    def nonEmptyMap[F[+a] <: Data.Optional[a], O <: Data, A: Order, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B],
        minProperties: Option[Int] = none,
        maxProperties: Option[Int] = none
    ): Dictionary.Required.Of[F[O], NonEmptyMap[A, B]] = Base.Dictionary
      .nonEmpty(key, value, minProperties, maxProperties)
      .imap { case (head, tail) => NonEmptyMap(head, SortedMap.from(tail)) }(fa => (fa.head, fa.tail.toVector))

  def enumeration[A, B](codec: Codec.Required.Of[Data.Primitive, A])(using
      mapping: Mapping[B, A]
  ): Enumeration.Required[B] = Base.Enumeration(codec, mapping)

  def enumeration[A: Order, B](codec: Codec.Required.Of[Data.Primitive, A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration.Required[B] = enumeration(codec)(using Mapping.enumeration(f))

  object dynamic:
    val any: Dynamic.Of[Data.Value, Data] = Base.Dynamic.Any
    val value: Dynamic.Required.Of[Data.Value, Data.Value] = Base.Dynamic.Value
    val obj: Dynamic.Required.Of[Data.Object[?], Data.Object[?]] = Base.Dynamic.Object
    val array: Dynamic.Required.Of[Data.Array[?], Data.Array[?]] = Base.Dynamic.Array
    val primitive: Dynamic.Required.Of[Data.Primitive, Data.Primitive] = Base.Dynamic.Primitive
    val number: Dynamic.Required.Of[Data.Number, Data.Number] = Base.Dynamic.Number
    val void: Dynamic.Required.Of[Data.Null.type, Data.Null.type] = Base.Dynamic.Null

  def singleton[A <: Singleton](a: A): Dynamic.Required.Of[Data.Null.type, A] =
    dynamic.void.imap(_ => a)(_ => Data.Null)

  val xpath: Primitive.Required[XPath] = parser(name = "xpath")(XPath.parse(_).toOption)(_.print)

object Codecs extends Codecs
