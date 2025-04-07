package io.taig.otter

import cats.~>
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import scala.collection.immutable.SortedSet
import cats.Invariant

sealed abstract class Collection[+S[_], A] extends Codec[S, A]:
  def codec: Reference[S, ?]
  def constraints: Vector[Constraint.Collection]
  override def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

object Collection:
  private def constraints(
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean
  ): Vector[Constraint.Collection] = Vector(
    minimum.map(Constraint.Collection.MinItems.apply),
    maximum.map(Constraint.Collection.MaxItems.apply),
    Option.when(uniqueItems)(Constraint.Collection.UniqueItems)
  ).flatten

  final private[otter] case class Indexed[S[_], A](
      codec: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, Vector[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, Vector[A]] = copy(codec = codec.mapK(fK))

  final private[otter] case class Linked[S[_], A](
      codec: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, List[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, List[A]] = copy(codec = codec.mapK(fK))

  final private[otter] case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, B] = copy(self = self.mapK(fK))

  given [S[_]]: Invariant[Collection[S, *]] with
    def imap[A, B](fa: Collection[S, A])(f: A => B)(g: B => A): Collection[S, B] = fa.imap(f)(g)
