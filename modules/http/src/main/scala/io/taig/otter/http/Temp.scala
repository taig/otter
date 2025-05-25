package io.taig.otter.http

import io.taig.otter.Enriched
import io.taig.otter.schema.EnrichedSchema
import io.taig.otter.Metadata

object Temp:
  type Body[+S[_], A] = Enriched[io.taig.otter.http.Body[S, *], A]

  given [S[_]]: EnrichedSchema[Body[S, *]] = new EnrichedSchema[Body[S, *]]:
    extension [A](self: Body[S, A])
      override def metadata: Metadata = ???
      override def metadata(f: Metadata => Metadata): Body[S, A] = ???

    override def imap[A, B](fa: Body[S, A])(f: A => B)(g: B => A): Body[S, B] = ???

  def test[S[_]] =
    summon[EnrichedSchema[Body[S, *]]]
