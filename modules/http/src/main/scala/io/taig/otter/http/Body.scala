package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.operation.*

// TODO strict vs streaming (?)
final case class Body[+S[_], A](self: Enrichment[Body.Value[S, A]]) extends AnyVal:
  inline def value: Body.Value[S, A] = self.self

  def mediaType: MediaType = value.mediaType
  def satisfies(mediaRange: MediaRange): Boolean = value.satisfies(mediaRange)
  def matches(contentType: MediaType): Boolean = value.matches(contentType)

  def schema: Reference[S, ?] = value.schema

  def :+[T[_], B](body: Body[T, B]): Bodies[S + T, Either[A, B]] = toBodies :+ body

  def +:[T[_], B](body: Body[T, B]): Bodies[S + T, Either[B, A]] = body.toBodies :+ this

  def toBodies: Bodies[S, A] = Bodies(Enrichment(Bodies.Value.Root(this)))

object Body:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def mediaType: MediaType
    def schema: Reference[S, ?]

    final def satisfies(mediaRange: MediaRange): Boolean = mediaType.satisfies(mediaRange)
    final def matches(contentType: MediaType): Boolean = mediaType === contentType

    final def imap[B](f: A => B)(g: B => A): Body.Value[S, B] = Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Body.Value[S, A], f: A => B, g: B => A)
        extends Body.Value[S, B]:
      export self.{mediaType, schema}

    final private[otter] case class Root[S[_], A](mediaType: MediaType, schema: Reference[S, A])
        extends Body.Value[S, A]

  extension [S[_], A <: Matchable](self: Body[S, A])
    inline def |[T[_], B <: Matchable](body: Body[T, B]): Bodies[S + T, A | B] = self.toBodies | body

  given [S[_]]: EnrichedSchemaInvariant[Body[S, *]] with
    override def imap[A, B](fa: Body[S, A])(f: A => B)(g: B => A): Body[S, B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Body[S, A])
      override def metadata: Metadata = self.self.metadata
      override def metadata(f: Metadata => Metadata): Body[S, A] = self.copy(self = self.self.modifyMetadata(f))
