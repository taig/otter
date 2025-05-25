package io.taig.otter.http.schema

import io.taig.otter.http.header.MediaType
import io.taig.otter.+
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import cats.syntax.all.*
import io.taig.otter.Enrichment
import io.taig.otter.schema.Schema

trait BodySchema[Self[+_[_], _]] extends SchemaK[Self]:
  self =>

  def apply[S[_], A](mediaType: MediaType, schema: => S[A]): Self[S, A]

  extension [S[_], A](self: Self[S, A])
    def mediaType: MediaType

    def schema: Reference[S, ?]

    final def satisfies(mediaRange: MediaRange): Boolean = mediaType.satisfies(mediaRange)

    final def matches(contentType: MediaType): Boolean = mediaType === contentType

object BodySchema:
  given schema[Self[+_[_], _]](using self: BodySchema[Self]): BodySchema[[s[_], a] =>> Enrichment[Self[s, *], a]] with
    override def apply[S[_], A](mediaType: MediaType, schema: => S[A]): Enrichment[Self[S, *], A] =
      Enrichment(self(mediaType, schema))

    extension [S[_], A](self: Enrichment[Self[S, *], A])
      override def mediaType: MediaType = self.self.mediaType
      override def schema: Reference[S, ?] = self.self.schema

    override def schema[S[_]]: Schema[Enrichment[Self[S, *], *]] = self
      .schema[S]
      .imapK([A] => (self: Self[S, A]) => Enriched(self))(
        [A] => (self: Enrichment[Self[S, *], A]) => self.self
      )
