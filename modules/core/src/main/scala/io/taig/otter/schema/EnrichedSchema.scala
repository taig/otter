package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Metadata.*

trait EnrichedSchema[Self[_]] extends Schema[Self]:
  self =>

  extension [A](self: Self[A])
    def metadata: Metadata
    def metadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      metadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = metadata(_.put(key, value))

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedSchema[T] =
    new EnrichedSchema[T]:
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

object EnrichedSchema:
  inline def apply[Self[_]](using self: EnrichedSchema[Self]): EnrichedSchema[Self] = self
