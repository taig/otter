package io.taig.otter.http

sealed abstract class Request[+S, A] extends Product with Serializable:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Option[Body[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Request[S, B] = Request.Modify(self = this, f, g)

  final def zip[B](headers: Headers[B]): Request[S, (A, B)] = Request.ZipHeaders(self = this, headers)

  final def zip[B](url: Url[B]): Request[S, (A, B)] = Request.ZipUrl(self = this, url)

object Request:
  final private[otter] case class Modify[S, A, B](self: Request[S, A], f: A => B, g: B => A) extends Request[S, B]:
    export self.{body, headers, method, url}

  final private[otter] case class Root[S, A, B, C, D](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Option[Body[S, D]]
  ) extends Request[S, (A, B, C, D)]

  final private[otter] case class ZipHeaders[S, A, B](self: Request[S, A], headers: Headers[B])
      extends Request[S, (A, B)]:
    export self.{body, method, url}

  final private[otter] case class ZipUrl[S, A, B](self: Request[S, A], url: Url[B]) extends Request[S, (A, B)]:
    export self.{body, headers, method}
