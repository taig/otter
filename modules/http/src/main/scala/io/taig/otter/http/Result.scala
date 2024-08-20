package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import org.typelevel.ci.*
import io.taig.otter.Convert
import io.taig.otter.http.header.Accept
import cats.data.NonEmptyList
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.Parameters
import java.nio.charset.Charset

sealed abstract class Result[A]:
  self =>

  def code: Code
  def headers: Headers[?]
  def bodies: Option[Bodies[?]]

  final def imap[B](f: A => B)(g: B => A): Result[B] = new Result[B]:
    export self.{bodies, code, headers}
    override def decode(response: Http.Response): Codec.Result[Option[B]] = self.decode(response).map(_.map(f))
    override def encode(accept: Accept.Result, b: B): Option[Http.Response] = self.encode(accept, g(b))
    override def encode(charset: Option[Charset], b: B): Http.Response = self.encode(charset, g(b))

  final def to[B](using convert: Convert[A, B]): Result[B] = imap(convert.to)(convert.from)

  final def orElse[B](result: Result[B]): Results[Either[A, B]] = toResults.orElse(result.toResults)

  final def toResults: Results[A] = Results(this)

  final def :+[B](result: Result[B]): Results[Either[A, B]] = orElse(result)

  final def +:[B](result: Result[B]): Results[Either[B, A]] = result :+ this

  def decode(response: Http.Response): Codec.Result[Option[A]]

  def encode(accept: Accept.Result, a: A): Option[Http.Response]

  def encode(charset: Option[Charset], a: A): Http.Response

object Result:
  extension [A <: Matchable](self: Result[A])
    inline def |[B <: Matchable](result: Result[B]): Results[A | B] = (self :+ result).imap {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

  def apply[A, B](code: Code, headers: Headers[A], bodies: Bodies[B]): Result[(A, B)] =
    val _code = code
    val _headers = headers
    val _bodies = bodies

    new Result[(A, B)]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = Some(_bodies)
      override def decode(response: Http.Response): Codec.Result[Option[(A, B)]] =
        if code =!= response.code then none.valid
        else ??? // (headers.decode(response.headers), _bodies.decode(???, response.body)).tupled
      override def encode(accept: Accept.Result, ab: (A, B)): Option[Http.Response] =
        val (blocklist, acceptlist) = accept.fold(
          left => (left.toList, List.empty),
          right => (List.empty, right.toList),
          (left, right) => (left.toList, right.toList)
        )

        acceptlist.toNel
          .getOrElse(NonEmptyList.one(MediaRange(MediaRange.Type.Any, Parameters.Empty)))
          .collectFirstSome(_bodies.encode(_, blocklist, ab._2))
          .map { case (mediaType, payload) =>
            Http.Response(code, (ci"Content-Type", mediaType.show) +: headers.encode(ab._1), payload)
          }
      override def encode(charset: Option[Charset], ab: (A, B)): Http.Response =
        val (mediaType, payload) = _bodies.encodeFirst(charset, ab._2)
        Http.Response(code, (ci"Content-Type", mediaType.show) +: headers.encode(ab._1), payload)

  def apply[A](code: Code, headers: Headers[A]): Result[A] =
    val _code = code
    val _headers = headers

    new Result[A]:
      override def code: Code = _code
      override def headers: Headers[A] = _headers
      override def bodies: Option[Bodies[?]] = none
      override def decode(response: Http.Response): Codec.Result[Option[A]] =
        if code =!= response.code then none.valid else headers.decode(response.headers).map(_.some)
      override def encode(accept: Accept.Result, a: A): Option[Http.Response] = encode(charset = none, a).some
      override def encode(charset: Option[Charset], a: A): Http.Response =
        Http.Response(code, headers.encode(a), Array.emptyByteArray)
