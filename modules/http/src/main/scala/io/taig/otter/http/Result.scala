package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import org.typelevel.ci.*
import io.taig.otter.Convert

sealed abstract class Result[A]:
  self =>

  def code: Code
  def headers: Headers[?]
  def bodies: Option[Bodies[?]]

  final def imap[B](f: A => B)(g: B => A): Result[B] = new Result[B]:
    export self.{bodies, code, headers}
    override def unsafeDecode[F[_]](response: Http.Response[F]): Codec.Result[B] =
      self.unsafeDecode(response).map(f)
    override def encode[F[_]](b: B): Http.Response[F] = self.encode(g(b))

  final def orElse[B](result: Result[B]): Results[Either[A, B]] = toResults.orElse(result.toResults)

  final def toResults: Results[A] = Results(this)

  final def :+[B](result: Result[B]): Results[Either[A, B]] = orElse(result)

  final def +:[B](result: Result[B]): Results[Either[B, A]] = result :+ this

  final def to[B](using convert: Convert[A, B]): Result[B] = imap(convert.to)(convert.from)

  final def decode[F[_]](response: Http.Response[F]): Codec.Result[Option[A]] =
    if code =!= response.code then none.valid else unsafeDecode(response).map(_.some)
  def unsafeDecode[F[_]](response: Http.Response[F]): Codec.Result[A]
  def encode[F[_]](a: A): Http.Response[F]

object Result:
  def apply[A, B](code: Code, headers: Headers[A], bodies: Bodies[B]): Result[(A, B)] =
    val _code = code
    val _headers = headers
    val _bodies = bodies

    new Result[(A, B)]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = Some(_bodies)
      override def unsafeDecode[F[_]](response: Http.Response[F]): Codec.Result[(A, B)] =
        // (headers.decode(response.headers), _bodies.decode(???, response.body)).tupled
        ???
      override def encode[F[_]](ab: (A, B)): Http.Response[F] =
        val (mediaType, payload) = _bodies.encode(ab._2)
        Http.Response(code, (ci"Content-Type", mediaType.show) +: headers.encode(ab._1), payload)

  def apply[A](code: Code, headers: Headers[A]): Result[A] =
    val _code = code
    val _headers = headers

    new Result[A]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = none
      override def unsafeDecode[F[_]](response: Http.Response[F]): Codec.Result[A] =
        headers.decode(response.headers)
      override def encode[F[_]](a: A): Http.Response[F] =
        ??? // Http.Response(code, headers.encode(a), Http.Payload.Empty)
