package io.taig.otter

import io.taig.otter.schema.EnrichedSchema
import cats.syntax.all.*

final case class Enriched[S[_], A](self: S[A], metadata: Metadata):
  def mapF[T[_], B](f: S[A] => T[B]): Enriched[T, B] = copy(self = f(self))

object Enriched:
  def apply[S[_], A](self: S[A]): Enriched[S, A] = Enriched(self, metadata = Metadata.Empty)

  given schema[S[_]]: EnrichedSchema[Enriched[S, *]] = new EnrichedSchema[Enriched[S, *]]:
    extension [A](self: Enriched[S, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Enriched[S, A] = self.copy(metadata = f(self.metadata))
