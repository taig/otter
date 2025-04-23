package io.taig.otter.http

sealed abstract class Request[+S, A] extends Product with Serializable:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Body[S, ?]

  final def imap[B](f: A => B)(g: B => A): Request[S, B] = Request.Modify(self = this, f, g)

  final def zip[B](headers: Headers[B]): Request[S, (A, B)] = Request.ZipHeaders(self = this, headers)

object Request:
  final private[otter] case class Modify[S, A, B](self: Request[S, A], f: A => B, g: B => A) extends Request[S, B]:
    export self.{body, headers, method, url}

  final private[otter] case class Root[S, A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Body[S, C]
  ) extends Request[S, (A, B, C)]

  final private[otter] case class ZipHeaders[S, A, B](self: Request[S, A], headers: Headers[B])
      extends Request[S, (A, B)]:
    export self.{body, method, url}
