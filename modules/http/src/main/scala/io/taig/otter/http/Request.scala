package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.http.Request.Body
import io.taig.otter.Codec.Result

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

  def decode(value: Http.Request): Codec.Result[A]

  def encode(a: A): Http.Request

object Request:
  sealed abstract class Body[A]:
    def decode(body: Http.Request.Body): Codec.Result[A]

    def encode(a: A): Http.Request.Body

  object Body:
    sealed abstract class Singlepart[A] extends Request.Body[A]

    object Singlepart:
      sealed abstract class Strict[A] extends Request.Body.Singlepart[A]:
        final def optional: Request.Body.Singlepart.Strict[Option[A]] = ???

        final def orElse[B](body: Request.Body.Singlepart.Strict[B]): Request.Body.Singlepart.Strict[Either[A, B]] = ???

      object Strict:
        val Empty: Request.Body.Singlepart.Strict[Unit] = ???

        val Binary: Request.Body.Singlepart.Strict[Array[Byte]] = ???

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
