package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.operation.*

// TODO strict vs streaming (?)
final case class Body[+S[_], A](value: Body.Value[S, A], metadata: Metadata):
  def mediaType: MediaType = value.mediaType
  def satisfies(mediaRange: MediaRange): Boolean = value.satisfies(mediaRange)
  def matches(contentType: MediaType): Boolean = value.matches(contentType)

  def schema: Reference[S, ?] = value.schema

  def :+[T[_], B](body: Body[T, B]): Bodies[S + T, Either[A, B]] = toBodies :+ body

  def +:[T[_], B](body: Body[T, B]): Bodies[S + T, Either[B, A]] = body.toBodies :+ this

  def toBodies: Bodies[S, A] = Bodies(value = Bodies.Value.Root(this), metadata = Metadata.Empty)

object Body:
  sealed abstract class Value[+S[_], A] extends Product, Serializable:
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

  given [S[_]]: SchemaInvariant[Body[S, *]] with
    override def imap[A, B](fa: Body[S, A])(f: A => B)(g: B => A): Body[S, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Body[S, A]] = new Enriched[Body[S, A]]:
      override def metadata(a: Body[S, A]): Metadata = a.metadata
      override def modifyMetadata(a: Body[S, A])(f: Metadata => Metadata): Body[S, A] =
        a.copy(metadata = f(a.metadata))
