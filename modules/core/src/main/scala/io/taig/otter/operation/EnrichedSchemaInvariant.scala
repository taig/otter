package io.taig.otter.operation

import io.taig.otter.Metadata
import io.taig.otter.Metadata.*
import io.taig.otter.Enrichment
import cats.Invariant
import cats.syntax.all.*

trait EnrichedSchemaInvariant[Self[_]] extends SchemaInvariant[Self]:
  self =>

  extension [A](self: Self[A])
    def metadata: Metadata
    def metadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      metadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = metadata(_.put(key, value))

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedSchemaInvariant[T] =
    new EnrichedSchemaInvariant[T]:
      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object EnrichedSchemaInvariant:
  inline def apply[Self[_]](using self: EnrichedSchemaInvariant[Self]): EnrichedSchemaInvariant[Self] = self

  given [Self[_]: Invariant]: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]] with
    extension [A](self: Enrichment[Self[A]])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Enrichment[Self[A]] = self.modifyMetadata(f)

    override def imap[A, B](fa: Enrichment[Self[A]])(f: A => B)(g: B => A): Enrichment[Self[B]] = fa.map(_.imap(f)(g))
