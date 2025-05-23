package io.taig.otter.http

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.schema.Schema

sealed abstract class Request[+S[_], A] extends Product with Serializable:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def bodies: Option[Bodies[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Request[S, B] = Request.Modify(self = this, f, g)

  final def zip[B](headers: Headers[B]): Request[S, (A, B)] = Request.ZipHeaders(self = this, headers)

object Request:
  final private[otter] case class Modify[S[_], A, B](self: Request[S, A], f: A => B, g: B => A) extends Request[S, B]:
    export self.{bodies, headers, method, url}

  final private[otter] case class Payload[S[_], A, B, C](self: Request.Root[A, B], payload: Bodies[S, C])
      extends Request[S, (A, B, C)]:
    export self.{headers, method, url}
    override def bodies: Option[Bodies[S, C]] = payload.some

  final private[otter] case class Root[A, B](
      method: Method,
      url: Url[A],
      headers: Headers[B]
  ) extends Request[Nothing, (A, B)]:
    override def bodies: Option[Bodies[Nothing, ?]] = none

  final private[otter] case class ZipHeaders[S[_], A, B](self: Request[S, A], headers: Headers[B])
      extends Request[S, (A, B)]:
    export self.{bodies, method, url}

  final case class Data(method: Method, url: Url.Data, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Data = modifyBody(_ => body)

  given [S[_]]: Schema[Request[S, *]] with
    override def imap[A, B](fa: Request[S, A])(f: A => B)(g: B => A): Request[S, B] = fa.imap(f)(g)
