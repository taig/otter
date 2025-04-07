package io.taig.otter

import cats.~>
import cats.implicits.*
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector
import cats.Order
import scala.collection.immutable.SortedMap

sealed abstract class Dictionary[+S[_], +T[_], A] extends Codec[T, A]:
  def key: Reference[S, ?]
  def value: Reference[T, ?]
  def constraints: Vector[Constraint.Object]
  override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, A]
  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, A]
  def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, A]
  final override def imap[B](f: A => B)(g: B => A): Dictionary[S, T, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference[S, A],
      value: Reference[T, B],
      minimum: Option[Int],
      maximum: Option[Int],
      metadata: Metadata
  ) extends Dictionary[S, T, List[(A, B)]]:
    override def constraints: Vector[Constraint.Object] = Vector(
      minimum.map(Constraint.Object.MinProperties.apply),
      maximum.map(Constraint.Object.MaxProperties.apply)
    ).flatten
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, List[(A, B)]] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, List[(A, B)]] = copy(value = value.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, List[(A, B)]] = copy(key = key.mapK(fK))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Dictionary[S, T, A], f: A => B, g: B => A)
      extends Dictionary[S, T, B]:
    export self.{constraints, key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, B] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, B] = copy(self = self.leftMapK(fK))

  trait Syntax[Self[_], Key[_], Value[_]] extends Codec.Syntax[Self]:
    def list[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[List[(A, B)]]

    final def nonEmptyList[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def vector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    final def nonEmptyVector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    final def nonEmptySeq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    final def chain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    final def nonEmptyChain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    final def map[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    final def sortedMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    final def nonEmptyMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none
    ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.max(1.some), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

  object Syntax:
    trait Default[Self[_], Key[_], Value[_]] extends Syntax[Self, Key, Value]:
      def fromDictionary[A](dictionary: Dictionary[Key, Value, A]): Self[A]
      def toDictionary[A](self: Self[A]): Dictionary[Key, Value, A]

      final override def list[A, B](
          key: => Key[A],
          value: => Value[B],
          minimum: Option[Int] = none,
          maximum: Option[Int] = none
      ): Self[List[(A, B)]] = fromDictionary(
        Dictionary.Root(
          key = Reference.later(key),
          value = Reference.later(value),
          minimum,
          maximum,
          metadata = Metadata.Empty
        )
      )

      extension [A](self: Self[A])
        final override def imap[B](f: A => B)(g: B => A): Self[B] = fromDictionary(toDictionary(self).imap(f)(g))
        final override def metadata: Metadata = toDictionary(self).metadata
        final override def modifyMetadata(f: Metadata => Metadata): Self[A] = fromDictionary(
          toDictionary(self).modifyMetadata(f)
        )
