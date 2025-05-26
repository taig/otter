package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.Enrichment
import io.taig.otter.operation.*
import io.taig.otter.Metadata

// TODO strict vs streaming (?)
type Body[+S[_], A] = Enrichment[Body.Value[S, *], A]

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

  given [S[_]]: EnrichedSchemaInvariant[Body[S, *]] with
    override def imap[A, B](fa: Body[S, A])(f: A => B)(g: B => A): Body[S, B] = fa.mapF(_.imap(f)(g))

    extension [A](self: Body[S, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Body[S, A] = self.modifyMetadata(f)
