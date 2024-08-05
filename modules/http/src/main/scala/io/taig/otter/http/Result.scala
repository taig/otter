package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Evidence
import io.taig.otter.Codec
import io.taig.otter.http.Response.Body

sealed abstract class Result[A]:
  self =>

  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]

  final def imap[B](f: A => B)(g: B => A): Result[B] = new Result[B]:
    export self.{body, code, headers}
    override def decode(response: Http.Response): Codec.Result[Option[B]] =
      self.decode(response).map(_.map(f))
    override def encode(b: B): Http.Response = self.encode(g(b))

  final def orElse[B](result: Result[B]): Results[Either[A, B]] = toResults.orElse(result.toResults)

  final def toResults: Results[A] = Results(this)

  final def :+[B](result: Result[B]): Results[Either[A, B]] = orElse(result)

  final def +:[B](result: Result[B]): Results[Either[B, A]] = result :+ this

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Result[B] = imap(evidence.from)(evidence.to)

  def decode(response: Http.Response): Codec.Result[Option[A]]
  def encode(a: A): Http.Response

object Result:
  def apply[A, B](code: Code, headers: Headers[A], body: Response.Body[B]): Result[(A, B)] =
    val _code = code
    val _headers = headers
    val _body = body

    new Result[(A, B)]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def body: Response.Body[B] = _body
      override def decode(response: Http.Response): Codec.Result[Option[(A, B)]] =
        if code =!= response.code
        then none.valid
        else (headers.decode(response.headers), body.decode(response.body)).tupled.map(_.some)
      override def encode(ab: (A, B)): Http.Response =
        Http.Response(code, headers.encode(ab._1), body.encode(ab._2))
