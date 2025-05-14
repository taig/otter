package io.taig.otter.schema

import cats.implicits.*
import cats.~>
import io.taig.otter.Constraint
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Shape

sealed abstract class Collection[+S[_], A] extends Schema[S, A]:
  def constraints: Vector[Constraint.Collection]
  def schema: Reference[S, ?]

  override def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, A]

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
      schema: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, Vector[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, Vector[A]] = copy(schema = schema.mapK(fK))

  final private[otter] case class Linked[S[_], A](
      schema: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, List[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, List[A]] = copy(schema = schema.mapK(fK))

  final private[otter] case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{constraints, metadata, schema}
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, B] = copy(self = self.mapK(fK))

  given [Value[_]]: Shape.Collection[Collection[Value, *], Value] with
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
