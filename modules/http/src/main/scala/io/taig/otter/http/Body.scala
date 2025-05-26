package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.schema.BodySchema
import io.taig.otter.schema.Schema

// TODO strict vs streaming (?)
sealed abstract class Body[+S[_], A] extends Product with Serializable:
  def mediaType: MediaType

  def schema: Reference[S, ?]

  final def satisfies(mediaRange: MediaRange): Boolean = mediaType.satisfies(mediaRange)

  final def matches(contentType: MediaType): Boolean = mediaType === contentType

  final def imap[B](f: A => B)(g: B => A): Body[S, B] = Body.Modify(self = this, f, g)

object Body:
  final private[otter] case class Modify[S[_], A, B](self: Body[S, A], f: A => B, g: B => A) extends Body[S, B]:
    export self.{mediaType, schema}

  final private[otter] case class Root[S[_], A](mediaType: MediaType, schema: Reference[S, A]) extends Body[S, A]

  given BodySchema[Body] = ???
