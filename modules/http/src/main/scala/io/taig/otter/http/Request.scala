package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.http.Request.Body
import io.taig.otter.Codec.Result
import io.taig.otter.Evidence
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.Violation
import io.taig.otter.Constraint
import io.taig.otter.Data

sealed abstract class Request[A]:
  self =>

  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Request.Body[?]

  final def imap[B](f: A => B)(g: B => A): Request[B] = new Request[B]:
    export self.{body, headers, method, url}
    override def decode(value: Http.Request): Codec.Result[B] = self.decode(value).map(f)
    override def encode(b: B): Http.Request = self.encode(g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Request[B] = imap(evidence.from)(evidence.to)

  final def zip[B](headers: Headers[B]): Request[(A, B)] = ???

  final def :*[B](header: Header[B])(using merge: Evidence.Merge[A, B]): Request[merge.Out] =
    zip(header.toHeaders).imap(merge.apply)(merge.unapply)

  final def *:[B](header: Header[B])(using merge: Evidence.Merge[B, A]): Request[merge.Out] =
    zip(header.toHeaders).imap(ab => merge(ab.swap))(merge.unapply(_).swap)

  def decode(value: Http.Request): Codec.Result[A]

  def encode(a: A): Http.Request

object Request:
  sealed abstract class Body[A]:
    def mediaType: MediaType

    def decode(body: Http.Payload): Codec.Result[A]

    def encode(a: A): Http.Payload

  object Body:
    val Empty: Request.Body[Unit] = ???

    val Binary: Request.Body[Array[Byte]] = ???

    def apply[A](
        mediaType: MediaType,
        f: Array[Byte] => Validated[Violations[Violation[Constraint, Data]], Data],
        g: Data => Array[Byte],
        of: Codec[Data.Optional, Data, A]
    ): Request.Body[A] =
      val _mediaType = mediaType

      new Body[A]:
        override def mediaType: MediaType = _mediaType
        override def decode(body: Http.Payload): Codec.Result[A] = f(body.data).andThen(of.decode)
        override def encode(a: A): Http.Payload = Http.Payload(g(of.encode(a)))

  def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Request.Body[C]): Request[(A, B, C)] =
    val _method = method
    val _url = url
    val _headers = headers
    val _body = body

    new Request[(A, B, C)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def body: Body[C] = _body
      override def encode(abc: (A, B, C)): Http.Request =
        Http.Request(method, url.encode(abc._1), headers.encode(abc._2), body.encode(abc._3))
      override def decode(value: Http.Request): Codec.Result[(A, B, C)] =
        (url.decode(value.url), headers.decode(value.headers), body.decode(value.body)).tupled
