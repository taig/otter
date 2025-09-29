package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.header.MediaType
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.validation.Violation
import org.typelevel.ci.*

final case class Headers[A](value: Headers.Value[A], metadata: Metadata):
  def toChain: Chain[Header[?]] = value.toChain

  def zip[B](headers: Headers[B]): Headers[(A, B)] =
    Headers(value = value.zip(headers.value), metadata = Metadata.Empty)

  def merge[B](headers: Headers[B])(using m: Merge[A, B]): Headers[m.Out] = zip(headers).merged

  def :*[B](header: Header[B])(using m: Merge[A, B]): Headers[m.Out] = merge(header.toHeaders)

  def *:[B](header: Header[B])(using m: Merge[B, A]): Headers[m.Out] = header.toHeaders.merge(this)

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

  val Empty: Headers[Unit] = Headers(value = Headers.Value.Empty, metadata = Metadata.Empty)

  given SchemaInvariant[Headers] with
    override def imap[A, B](fa: Headers[A])(f: A => B)(g: B => A): Headers[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Headers[A]] = new Enriched[Headers[A]]:
      override def metadata(a: Headers[A]): Metadata = a.metadata
      override def modifyMetadata(a: Headers[A])(f: Metadata => Metadata): Headers[A] =
        a.copy(metadata = f(a.metadata))
