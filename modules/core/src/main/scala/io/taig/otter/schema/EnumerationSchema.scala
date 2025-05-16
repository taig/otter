package io.taig.otter.schema

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Metadata
import io.taig.otter.Reference

trait EnumerationSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

  extension [A](self: Self[A])
    def schema: Reference[Value, ?]
    def values: NonEmptyList[A]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnumerationSchema[T, Value] = new EnumerationSchema[T, Value]:

    override def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): T[B] =
      fK(self.apply(schema, mapping))

    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def schema: Reference[Value, ?] = self.schema(gK(ta))
      override def values: NonEmptyList[A] = self.values(gK(ta))
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object EnumerationSchema:
  inline def apply[Self[_], Value[_]](using self: EnumerationSchema[Self, Value]): EnumerationSchema[Self, Value] =
    self
