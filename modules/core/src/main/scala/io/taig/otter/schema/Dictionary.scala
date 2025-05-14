package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Constraint
import cats.~>
import io.taig.otter.Shape
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyMap
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import cats.~>
import io.taig.otter.Argument
import scala.collection.immutable.SortedSet
import java.util.UUID
import cats.kernel.Eq
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues
import scala.collection.immutable.SortedMap

sealed abstract class Dictionary[+S[_], +T[_], A] extends Schema[T, A]:
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

  trait Component[+Self[_], -Key[_], -Value[_]](using self: Shape.Dictionary[Self, Key, Value]):
    final def list[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[List[(A, B)]] = self.dictionary(key, value, minimum = minimum.toOption, maximum = maximum.toOption)

    final def nonEmptyList[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def vector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    final def nonEmptyVector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    final def nonEmptySeq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    final def chain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    final def nonEmptyChain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    final def map[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    final def sortedMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    final def nonEmptyMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

  given [Key[_], Value[_]]: Shape.Dictionary[Dictionary[Key, Value, *], Key, Value] with
    override def dictionary[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): Dictionary[Key, Value, List[(A, B)]] = Root(
      key = Reference.later(key),
      value = Reference.later(value),
      minimum,
      maximum,
      metadata = Metadata.Empty
    )

    extension [A](fa: Dictionary[Key, Value, A])
      override def imap[B](f: A => B)(g: B => A): Dictionary[Key, Value, B] = fa.imap(f)(g)
      override def modifyMetadata(f: Metadata => Metadata): Dictionary[Key, Value, A] = fa.modifyMetadata(f)
      override def metadata: Metadata = fa.metadata
