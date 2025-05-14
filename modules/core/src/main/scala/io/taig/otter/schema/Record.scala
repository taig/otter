package io.taig.otter.schema

import cats.data.Chain
import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Invariant

sealed abstract class Record[+S[_], A] extends Schema[S, A]:
  override def modifyMetadata(f: Metadata => Metadata): Record[S, A]

  def isOptional: Boolean

  def fields: Chain[Reference[S, ?]]

  def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, A]

  final def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)

  final def optional: Record[S, Option[A]] = Record.Optional(self = this)

  final def zip[S1[a] >: S[a], B](codec: Record[S1, B]): Record[S1, (A, B)] =
    Record.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def isOptional: Boolean = false
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[_] >: Nothing, U[_]](fK: S1 ~> U): Record[U, Unit] = this

  final private[otter] case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.{fields, isOptional, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Optional[S[_], A](self: Record[S, A]) extends Record[S, Option[A]]:
    export self.{fields, metadata}
    override def isOptional: Boolean = true
    override def modifyMetadata(f: Metadata => Metadata): Record[S, Option[A]] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, Option[A]] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A](field: Reference[S, A], metadata: Metadata) extends Record[S, A]:
    override def isOptional: Boolean = false
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, A] = copy(field = field.mapK(fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Record[S, A],
      right: Record[S, B],
      metadata: Metadata
  ) extends Record[S, (A, B)]:
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  trait Shape[Self[_], Field[_]] extends Schema.Shape[Self], Invariant.Product[Self]:
    def record[A](field: => Field[A]): Self[A]

    extension [A](self: Self[A]) def optional: Self[Option[A]]

  object Shape:
    def apply[Self[_], Field[_]](
        lift: [A] => (self: Record[Field, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Record[Field, A]
    ): Record.Shape[Self, Field] = new Shape[Self, Field]:
      final override def record[A](field: => Field[A]): Self[A] =
        lift(Root(field = Reference.later(field), metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def optional: Self[Option[A]] = lift(extract(self).optional)
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))
