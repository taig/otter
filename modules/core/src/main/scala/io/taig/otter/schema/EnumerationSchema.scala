package io.taig.otter.schema

import io.taig.enumeration.ext.Mapping
import io.taig.otter.Metadata

trait EnumerationSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

  // final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
  //     gK: [A] => T[A] => Self[A]
  // ): EnumerationSchema[T, Value] = new EnumerationSchema[T, Value]:
  //   override def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): T[B] =
  //     fK(self.apply(schema, mapping))

  //   extension [A](ta: T[A])
  //     override def metadata: Metadata = self.metadata(gK(ta))
  //     override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
  //     override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object EnumerationSchema:
  inline def apply[Self[_], Value[_]](using self: EnumerationSchema[Self, Value]): EnumerationSchema[Self, Value] =
    self
