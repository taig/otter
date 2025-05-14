package io.taig.otter.schema

import cats.data.NonEmptyList
import cats.~>
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Shape
import cats.Order
import io.taig.enumeration.ext.EnumerationValues

sealed abstract class Enumeration[+S[_], A] extends Schema[S, A]:
  def metadata: Metadata
  def schema: Reference[S, ?]
  def values: NonEmptyList[A]
  def modifyMetadata(f: Metadata => Metadata): Enumeration[S, A]
  final def imap[B](f: A => B)(g: B => A): Enumeration[S, B] = Enumeration.Modify(self = this, f, g)
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, A]

object Enumeration:
  final private[otter] case class Modify[S[_], A, B](self: Enumeration[S, A], f: A => B, g: B => A)
      extends Enumeration[S, B]:
    export self.{metadata, schema}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A, B](
      schema: Reference[S, A],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[S, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, B] = copy(schema = schema.mapK(fK))

  trait Component[+Self[_], -Value[_]](using self: Shape.Enumeration[Self, Value]):
    final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
      self.enumeration(codec, mapping)

    final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
      enumeration(codec)(using Mapping.enumeration(f))

  given [Value[_]]: Shape.Enumeration[Enumeration[Value, *], Value] with
    override def enumeration[A, B](
        schema: => Value[A],
        mapping: Mapping[B, A]
    ): Enumeration[Value, B] = Root(
      schema = Reference.later(schema),
      mapping,
      metadata = Metadata.Empty
    )

    extension [A](fa: Enumeration[Value, A])
      override def imap[B](f: A => B)(g: B => A): Enumeration[Value, B] = fa.imap(f)(g)
      override def modifyMetadata(f: Metadata => Metadata): Enumeration[Value, A] = fa.modifyMetadata(f)
      override def metadata: Metadata = fa.metadata
