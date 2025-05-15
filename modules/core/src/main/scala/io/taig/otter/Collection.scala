package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import io.taig.otter.Argument
import io.taig.otter.Metadata

import scala.collection.immutable.SortedSet
import io.taig.otter.schema.CollectionSchema

sealed abstract class Collection[+S[_], A] extends Product with Serializable:
  def constraints: Vector[Constraint.Collection]
  def schema: Reference[S, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  final def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, A]

object Collection:
  private def constraints(
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean
  ): Vector[Constraint.Collection] = Vector(
    minimum.map(Constraint.Collection.Minimum.apply),
    maximum.map(Constraint.Collection.Maximum.apply),
    Option.when(uniqueItems)(Constraint.Collection.Unique)
  ).flatten

  final private[otter] case class Indexed[S[_], A](
      schema: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, Vector[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, Vector[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final private[otter] case class Linked[S[_], A](
      schema: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, List[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, List[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final private[otter] case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{constraints, metadata, schema}
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, B] =
      copy(self = self.mapK[S1, T](fK))

  given [Value[_]]: CollectionSchema[Collection[Value, *], Value] with
    extension [A](self: Collection[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Collection[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Collection[Value, B] = self.imap(f)(g)

    override def linked[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        uniqueItems: Boolean
    ): Collection[Value, List[A]] =
      Linked(schema = Reference.later(schema), minimum, maximum, uniqueItems, metadata = Metadata.Empty)

    override def indexed[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        uniqueItems: Boolean
    ): Collection[Value, Vector[A]] =
      Indexed(schema = Reference.later(schema), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
