package io.taig.otter

import io.taig.otter as Base
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyChain
import cats.data.Chain
import cats.Order
import cats.implicits.*
import scala.collection.immutable.SortedSet
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.data.NonEmptyMap
import scala.collection.immutable.SortedMap
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues

trait Codecs extends Validations:
  self =>

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive(tpe)

  val bigDecimal: Primitive.Required[JBigDecimal] = primitive(Type.BigDecimal)
  val bigInteger: Primitive.Required[JBigInteger] = primitive(Type.BigInteger)
  val boolean: Primitive.Required[Boolean] = primitive(Type.Boolean)
  val double: Primitive.Required[Double] = primitive(Type.Double)
  val float: Primitive.Required[Float] = primitive(Type.Float)
  val int: Primitive.Required[Int] = primitive(Type.Int)
  val long: Primitive.Required[Long] = primitive(Type.Long)
  val string: Primitive.Required[String] = primitive(Type.String)

  // def branch[A](name: String, codec: Codec[A]): Branch.Of[codec.type, A] = Base.Branch(name, codec)

  def field[F[+a <: Data] <: Data.Optional[a], O <: Data.Value, A](
      name: String,
      codec: Base.Codec[F, O, A]
  ): Field.Of[F[O], A] =
    Base.Field(name, codec)

  def tuple[O <: Data, A](fields: Fields[O, A]): Tuple.Required.Of[O, A] = fields.toTuple
  def tuple[O <: Data, A](field: Field.Of[O, A]): Tuple.Required.Of[O, A] = tuple(field.toFields)

  // object collection:
  //   def vector[A](codec: Codec[A]): Collection.Of[codec.type, Vector[A]] = codec.toCollection

  //   def seq[A](codec: Codec[A]): Collection.Of[codec.type, Seq[A]] = vector(codec).imap(identity)(_.toVector)

  //   def nonEmptySeq[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptySeq[A]] =
  //     seq(codec).ivalidate(nonEmpty.collection.iterable)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

  //   def list[A](codec: Codec[A]): Collection.Of[codec.type, List[A]] = vector(codec).imap(_.toList)(_.toVector)

  //   def nonEmptyList[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptyList[A]] =
  //     list(codec).ivalidate(nonEmpty.collection.iterable)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

  //   def chain[A](codec: Codec[A]): Collection.Of[codec.type, Chain[A]] = vector(codec).imap(Chain.fromSeq)(_.toVector)

  //   def nonEmptyChain[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptyChain[A]] =
  //     chain(codec).ivalidate(nonEmpty.collection.chain)(_.toChain)

  //   def set[A: Order](codec: Codec[A]): Collection.Of[codec.type, Set[A]] =
  //     vector(codec).ivalidate_(uniqueItems(codec)).imap(_.toSet)(_.toVector)

  //   def sortedSet[A: Order](codec: Codec[A]): Collection.Of[codec.type, SortedSet[A]] =
  //     vector(codec).ivalidate_(uniqueItems(codec)).imap(SortedSet.from)(_.toVector)

  //   def nonEmptySet[A: Order](codec: Codec[A]): Collection.Of[codec.type, NonEmptySet[A]] =
  //     sortedSet(codec)
  //       .ivalidate(nonEmpty.collection.iterable) { case (a, as) => as + a }
  //       .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))

  // object dictionary:
  //   def list[A, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, List[(A, B)]] =
  //     Base.Dictionary(key, value)

  //   def vector[A, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, Vector[(A, B)]] =
  //     list(key, value).imap(_.toVector)(_.toList)

  //   def seq[A, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, Seq[(A, B)]] =
  //     list(key, value).imap(identity)(_.toList)

  //   def chain[A, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, Chain[(A, B)]] =
  //     list(key, value).imap(Chain.fromSeq)(_.toList)

  //   def map[A, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, Map[A, B]] =
  //     list(key, value).imap(Map.from)(_.toList)

  //   def sortedMap[A: Order, B](key: Value.Required[A], value: Codec[B]): Dictionary.Of[value.type, SortedMap[A, B]] =
  //     list(key, value).imap(SortedMap.from)(_.toList)

  //   def nonEmptyMap[A: Order, B](
  //       key: Value.Required[A],
  //       value: Codec[B]
  //   ): Dictionary.Of[value.type, NonEmptyMap[A, B]] = sortedMap(key, value)
  //     .ivalidate(nonEmpty.obj.map) { case (head, tail) => SortedMap.from(tail) + head }
  //     .imap(NonEmptyMap.apply)(fa => (fa.head, fa.tail))

  // def enumeration[A, B](codec: Value.Required[A])(using mapping: Mapping[B, A]): Enumeration.Required[B] =
  //   Base.Enumeration(codec, mapping)

  // def enumeration[A: Order, B](codec: Value.Required[A])(f: B => A)(using
  //     EnumerationValues.Aux[B, B]
  // ): Enumeration.Required[B] = enumeration(codec)(using Mapping.enumeration(f))

object Codecs extends Codecs
