package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.operation.*
import io.taig.otter.Merge
import io.taig.otter.Reference
import io.taig.otter.Metadata
import io.taig.otter.Enrichment

final case class Request[+S[_], A](self: Enrichment[Request.Value[S, A]]) extends AnyVal:
  inline def value: Request.Value[S, A] = self.self

  def method: Method = value.method
  def url: Url[?] = value.url
  def headers: Headers[?] = value.headers
  def bodies: Option[Bodies[S, ?]] = value.bodies

  def zip[B](headers: Headers[B]): Request[S, (A, B)] = Request(Enrichment(value.zip(headers)))

  def *[B](headers: Headers[B])(using merge: Merge[A, B]): Request[S, merge.Out] = zip(headers).merge

  def :*[B](header: Header[B])(using merge: Merge[A, B]): Request[S, merge.Out] = this * header.toHeaders

  def *:[B](header: Header[B])(using merge: Merge[A, B]): Request[S, merge.Out] = this * header.toHeaders

object Request:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def method: Method
    def url: Url[?]
    def headers: Headers[?]
    def bodies: Option[Bodies[S, ?]]

    final def imap[B](f: A => B)(g: B => A): Request.Value[S, B] = Request.Value.Modify(self = this, f, g)

    final def zip[B](headers: Headers[B]): Request.Value[S, (A, B)] = Request.Value.ZipHeaders(self = this, headers)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Request.Value[S, A], f: A => B, g: B => A)
        extends Request.Value[S, B]:
      export self.{bodies, headers, method, url}

    final private[otter] case class Payload[S[_], A, B, C](self: Request.Value.Root[A, B], payload: Bodies[S, C])
        extends Request.Value[S, (A, B, C)]:
      export self.{headers, method, url}
      override def bodies: Option[Bodies[S, C]] = payload.some

    final private[otter] case class Root[A, B](
        method: Method,
        url: Url[A],
        headers: Headers[B]
    ) extends Request.Value[Nothing, (A, B)]:
      override def bodies: Option[Nothing] = none

    final private[otter] case class ZipHeaders[S[_], A, B](self: Request.Value[S, A], headers: Headers[B])
        extends Request.Value[S, (A, B)]:
      export self.{bodies, method, url}

  final case class Data(method: Method, url: Url.Data, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Data = modifyBody(_ => body)

  given [S[_]]: EnrichedSchemaInvariant[Request[S, *]] with
    override def imap[A, B](fa: Request[S, A])(f: A => B)(g: B => A): Request[S, B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Request[S, A])
      def metadata: Metadata = self.metadata
      def metadata(f: Metadata => Metadata): Request[S, A] =
        self.copy(self = self.self.modifyMetadata(f))
