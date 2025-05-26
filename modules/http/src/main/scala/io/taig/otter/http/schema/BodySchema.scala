package io.taig.otter.http.schema

import io.taig.otter.http.header.MediaType
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import cats.syntax.all.*
import io.taig.otter.schema.Schema

trait BodySchema[Self[+_[_], _]] extends SchemaK[Self]:
  self =>

  def apply[S[_], A](mediaType: MediaType, schema: => Reference[S, A]): Self[S, A]

  override def imapK[T[+_[_],_]](fK: [S[_], A] => Self[S, A] => T[S, A])(gK: [S[_], A] => T[S, A] => Self[S, A]): BodySchema[T] = ???

  extension [S[_], A](self: Self[S, A])
    def mediaType: MediaType

    def schema: Reference[S, ?]

    final def satisfies(mediaRange: MediaRange): Boolean = mediaType.satisfies(mediaRange)

    final def matches(contentType: MediaType): Boolean = mediaType === contentType
