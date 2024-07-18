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

trait Codecs extends Validations:
  self =>

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive(tpe)

  val double: Primitive.Required[Double] = primitive(Type.Double)
  val int: Primitive.Required[Int] = primitive(Type.Int)
  val long: Primitive.Required[Long] = primitive(Type.Long)
  val string: Primitive.Required[String] = primitive(Type.String)

  object collection:
    def vector[A](codec: Codec[A]): Collection.Of[codec.type, Vector[A]] = codec.toCollection

    def seq[A](codec: Codec[A]): Collection.Of[codec.type, Seq[A]] = vector(codec).imap(identity)(_.toVector)

    def nonEmptySeq[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptySeq[A]] =
      seq(codec).ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

    def list[A](codec: Codec[A]): Collection.Of[codec.type, List[A]] = vector(codec).imap(_.toList)(_.toVector)

    def nonEmptyList[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptyList[A]] =
      list(codec).ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

    def chain[A](codec: Codec[A]): Collection.Of[codec.type, Chain[A]] = vector(codec).imap(Chain.fromSeq)(_.toVector)

    def nonEmptyChain[A](codec: Codec[A]): Collection.Of[codec.type, NonEmptyChain[A]] =
      chain(codec).ivalidate(self.nonEmptyChain)(_.toChain)

    def set[A: Order](codec: Codec[A]): Collection.Of[codec.type, Set[A]] =
      vector(codec).ivalidate_(uniqueItems(codec)).imap(_.toSet)(_.toVector)

    def sortedSet[A: Order](codec: Codec[A]): Collection.Of[codec.type, SortedSet[A]] =
      vector(codec).ivalidate_(uniqueItems(codec)).imap(SortedSet.from)(_.toVector)

    def nonEmptySet[A: Order](codec: Codec[A]): Collection.Of[codec.type, NonEmptySet[A]] =
      sortedSet(codec)
        .ivalidate(nonEmpty) { case (a, as) => as + a }
        .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))

object Codecs extends Codecs
