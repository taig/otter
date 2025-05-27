package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.header.MediaType
import org.typelevel.ci.*

final case class Headers[A](self: Enrichment[Headers.Value[A]]) extends AnyVal:
  inline def value: Headers.Value[A] = self.self

  def toChain: Chain[Header[?]] = value.toChain

  def zip[B](headers: Headers[B]): Headers[(A, B)] = Headers(Enrichment(value.zip(headers.value)))

  def *[B](headers: Headers[B])(using merge: Merge[A, B]): Headers[merge.Out] = this.zip(headers).merge

  def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] = this * header.toHeaders

  def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] = header.toHeaders * this

object Headers:
  sealed abstract class Value[A]:
    def toChain: Chain[Header[?]]

    final def imap[B](f: A => B)(g: B => A): Headers.Value[B] = Value.Modify(self = this, f, g)

    final def zip[B](headers: Headers.Value[B]): Headers.Value[(A, B)] = Value.Zip(left = this, right = headers)

  object Value:
    private[otter] object Empty extends Headers.Value[Unit]:
      override def toChain: Chain[Nothing] = Chain.empty

    final private[otter] case class Modify[A, B](self: Headers.Value[A], f: A => B, g: B => A) extends Headers.Value[B]:
      export self.toChain

    final private[otter] case class Optional[A](self: Headers.Value[A]) extends Headers.Value[Option[A]]:
      export self.toChain

    final private[otter] case class Root[A](header: Header[A]) extends Headers.Value[A]:
      override def toChain: Chain[Header[A]] = Chain.one(header)

    final private[otter] case class Zip[A, B](left: Value[A], right: Value[B]) extends Value[(A, B)]:
      override def toChain: Chain[Header[?]] = left.toChain ++ right.toChain

  type Data = Chain[Header.Data]

  object Data:
    extension (self: Headers.Data)
      def apply(name: CIString): Option[String] = self.collectFirst { case (`name`, value) => value }

      def accept: Either[Violations, Option[Accept]] = self(ci"Accept").traverse: value =>
        Accept
          .parse(value)
          .leftMap: error =>
            Violations.rootNec(Violation.tpe(name = "Accept", actual = value, hint = error.show))

      def contentType: Either[Violations, Option[MediaType]] = self(ci"Content-Type").traverse: value =>
        MediaType
          .parse(value)
          .leftMap: error =>
            Violations.rootNec(Violation.tpe(name = "Content-Type", actual = value, hint = error.show))

  val Empty: Headers[Unit] = Headers(Enrichment(Headers.Value.Empty))

  given EnrichedSchemaInvariant[Headers] with
    override def imap[A, B](fa: Headers[A])(f: A => B)(g: B => A): Headers[B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Headers[A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Headers[A] =
        self.copy(self = self.self.modifyMetadata(f))
