package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.operation.*
import io.taig.otter.Reference
import io.taig.otter.Enrichment

type Request[+S[_], A] = Enrichment[Request.Value[S, *], A]

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

  extension [S[_], A](self: Request[S, A])
    def method: Method = self.self.method
    def url: Url[?] = self.self.url
    def headers: Headers[?] = self.self.headers
    def bodies: Option[Bodies[S, ?]] = self.self.bodies

  final case class Data(method: Method, url: Url.Data, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Data = modifyBody(_ => body)
