package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Evidence
import io.taig.otter.Codec
import java.nio.charset.StandardCharsets
import org.typelevel.ci.*

sealed abstract class Result[A]:
  self =>

  def code: Code
  def headers: Headers[?]
  def bodies: Option[Bodies[?]]

  final def imap[B](f: A => B)(g: B => A): Result[B] = new Result[B]:
    export self.{bodies, code, headers}
    override def unsafeDecode(response: Http.Response): Codec.Result[B] =
      self.unsafeDecode(response).map(f)
    override def encode(b: B): Http.Response = self.encode(g(b))

  final def orElse[B](result: Result[B]): Results[Either[A, B]] = toResults.orElse(result.toResults)

  final def toResults: Results[A] = Results(this)

  final def :+[B](result: Result[B]): Results[Either[A, B]] = orElse(result)

  final def +:[B](result: Result[B]): Results[Either[B, A]] = result :+ this

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Result[B] = imap(evidence.from)(evidence.to)

  final def decode(response: Http.Response): Codec.Result[Option[A]] =
    if code =!= response.code then none.valid else unsafeDecode(response).map(_.some)
  def unsafeDecode(response: Http.Response): Codec.Result[A]
  def encode(a: A): Http.Response

object Result:
  def apply[A, B](code: Code, headers: Headers[A], bodies: Bodies[B]): Result[(A, B)] =
    val _code = code
    val _headers = headers
    val _bodies = bodies

    new Result[(A, B)]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = Some(_bodies)
      override def unsafeDecode(response: Http.Response): Codec.Result[(A, B)] =
        // (headers.decode(response.headers), _bodies.decode(???, response.body)).tupled
        ???
      override def encode(ab: (A, B)): Http.Response = ???
      // val (mediaType, payload) = _bodies.encode(charset = none, ab._2)
      // Http.Response(code, (ci"Content-Type", mediaType.print) +: headers.encode(ab._1), payload)

  def apply[A](code: Code, headers: Headers[A]): Result[A] =
    val _code = code
    val _headers = headers

    new Result[A]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = none
      override def unsafeDecode(response: Http.Response): Codec.Result[A] =
        headers.decode(response.headers)
      override def encode(a: A): Http.Response = Http.Response(code, headers.encode(a), Http.Payload.Empty)
