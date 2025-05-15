package io.taig.otter

import cats.Order
import cats.data.NonEmptyList
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Metadata
import io.taig.otter.schema.EnumerationSchema

sealed abstract class Enumeration[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  def values: NonEmptyList[A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Enumeration[S, A]
  final def imap[B](f: A => B)(g: B => A): Enumeration[S, B] = Enumeration.Modify(self = this, f, g)
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, A]

object Enumeration:
  final private[otter] case class Modify[S[_], A, B](self: Enumeration[S, A], f: A => B, g: B => A)
      extends Enumeration[S, B]:
    export self.{metadata, schema}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A, B](
      schema: Reference[S, A],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[S, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, B] =
      copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: EnumerationSchema[Enumeration[Value, *], Value] with
    override def apply[A, B](
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
