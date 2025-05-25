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

  override def imapK[T[+_[_], _]](fK: [S[_], A] => Self[S, A] => T[S, A])(
      gK: [S[_], A] => T[S, A] => Self[S, A]
  ): BodySchema[T] = new BodySchema[T]:
    override def apply[S[_], A](mediaType: MediaType, schema: => S[A]): T[S, A] = fK(self(mediaType, schema))

    extension [S[_], A](ta: T[S, A])
      override def mediaType: MediaType = self.mediaType(gK(ta))
      override def schema: Reference[S, ?] = self.schema(gK(ta))

    override def algebra[S[_]]: Schema[T[S, *]] = self
      .algebra[S]
      .imapK(
        [A] => (self: Self[S, A]) => fK(self)
      )([A] => (value: T[S, A]) => gK(value))
