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

  def branch[F[+a <: Data] <: Data.Optional[a], O <: Data.Value, A](
      name: String,
      codec: Base.Codec[F, O, A]
  ): Branch.Of[F[O], A] = Base.Branch(name, codec)

  def field[F[+a <: Data] <: Data.Optional[a], O <: Data.Value, A](
      name: String,
      codec: Base.Codec[F, O, A]
  ): Field.Of[F[O], A] = Base.Field(name, codec)

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
    def vector[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], Vector[A]] = Base.Collection(codec)

    def nonEmptyVector[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], NonEmptyVector[A]] = vector(codec)
      .ivalidate(nonEmpty.collection.iterable)(_ +: _)
      .imap(NonEmptyVector.apply)(fa => (fa.head, fa.tail))

    def seq[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], Seq[A]] = vector(codec).imap(identity)(_.toVector)

    def nonEmptySeq[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], NonEmptySeq[A]] = seq(codec)
      .ivalidate(nonEmpty.collection.iterable)(_ +: _)
      .imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

    def list[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], List[A]] = vector(codec).imap(_.toList)(_.toVector)

    def nonEmptyList[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], NonEmptyList[A]] =
      list(codec).ivalidate(nonEmpty.collection.iterable)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

    def chain[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], Chain[A]] = vector(codec).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
        codec: Base.Codec[F, O, A]
    ): Collection.Of[F[O], NonEmptyChain[A]] =
      chain(codec).ivalidate(nonEmpty.collection.chain)(_.toChain)

    def set[F[+a <: Data] <: Data.Optional[a], O <: Data, A: Order](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], Set[A]] =
      vector(codec).ivalidate_(uniqueItems(codec)).imap(_.toSet)(_.toVector)

    def sortedSet[F[+a <: Data] <: Data.Optional[a], O <: Data, A: Order](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], SortedSet[A]] =
      vector(codec).ivalidate_(uniqueItems(codec)).imap(SortedSet.from)(_.toVector)

    def nonEmptySet[F[+a <: Data] <: Data.Optional[a], O <: Data, A: Order](
        codec: Base.Codec[F, O, A]
    ): Collection.Required.Of[F[O], NonEmptySet[A]] = sortedSet(codec)
      .ivalidate(nonEmpty.collection.iterable) { case (a, as) => as + a }
      .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))

  object dictionary:
    def vector[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], Vector[(A, B)]] = Base.Dictionary(key, value)

    def list[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], List[(A, B)]] = vector(key, value).imap(_.toList)(_.toVector)

    def seq[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], Seq[(A, B)]] = vector(key, value).imap(identity)(_.toVector)

    def chain[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], Chain[(A, B)]] = vector(key, value).imap(Chain.fromSeq)(_.toVector)

    def map[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], Map[A, B]] = vector(key, value).imap(Map.from)(_.toVector)

    def sortedMap[F[+a <: Data] <: Data.Optional[a], O <: Data, A: Order, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], SortedMap[A, B]] = vector(key, value).imap(SortedMap.from)(_.toVector)

    def nonEmptyMap[F[+a <: Data] <: Data.Optional[a], O <: Data, A: Order, B](
        key: Codec.Required.Of[Data.Primitive, A],
        value: Base.Codec[F, O, B]
    ): Dictionary.Required.Of[F[O], NonEmptyMap[A, B]] = sortedMap(key, value)
      .ivalidate(nonEmpty.obj.map) { case (head, tail) => SortedMap.from(tail) + head }
      .imap(NonEmptyMap.apply)(fa => (fa.head, fa.tail))

  def enumeration[A, B](codec: Codec.Required.Of[Data.Primitive, A])(using
      mapping: Mapping[B, A]
  ): Enumeration.Required[B] =
    Base.Enumeration(codec, mapping)

  def enumeration[A: Order, B](codec: Codec.Required.Of[Data.Primitive, A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration.Required[B] = enumeration(codec)(using Mapping.enumeration(f))

object Codecs extends Codecs
