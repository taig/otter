package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Metadata.*
import io.taig.otter.Enrichment
import cats.Invariant
import cats.syntax.all.*

trait EnrichedSchema[Self[_]] extends Schema[Self]:
  extension [A](self: Self[A])
    def metadata: Metadata
    def metadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      metadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = metadata(_.put(key, value))

object EnrichedSchema:
  inline def apply[Self[_]](using self: EnrichedSchema[Self]): EnrichedSchema[Self] = self

  given [Self[_]: Invariant]: EnrichedSchema[[a] =>> Enrichment[Self, a]] with
    extension [A](self: Enrichment[Self, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Enrichment[Self, A] = self.modifyMetadata(f)

    override def imap[A, B](fa: Enrichment[Self, A])(f: A => B)(g: B => A): Enrichment[Self, B] = fa.mapF(_.imap(f)(g))
