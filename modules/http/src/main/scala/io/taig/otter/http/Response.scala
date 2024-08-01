package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.{Codec, Data}
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.Constraint

final case class Response[A](
    results: Results[A],
    violations: Result[Violations[Violation[Constraint.Any[Data], Data]]]
):
  def decode(response: Http.Response): Codec.Result[A] = results.decode(response)

  def encode(a: Validated[Violations[Violation[Constraint.Any[Data], Data]], A]): Http.Response =
    a.fold(violations.encode, results.encode)

object Response:
  sealed abstract class Body[A]:
    self =>

    def decode(headers: Http.Headers, payload: Http.Payload): Codec.Result[A]

    def encode(a: A): (Http.Headers, Http.Payload)

  object Body:
    sealed abstract class Strict[A] extends Response.Body[A]:
      self =>

      override def decode(headers: Http.Headers, payload: Http.Payload): Codec.Result[A] = ???
      // payload match
      //   case Http.Payload.Strict(data) => decode(headers, data)
      //   case Http.Payload.Streaming(_) =>
      //     Violations.oneNec(History.Root / "body", Violation.tpe("strict", "streaming")).invalid
      // def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A]
      override def encode(a: A): (Http.Headers, Http.Payload.Strict)

    // object Strict:
    // sealed abstract class Payload[A](val codec: Codec[?], val mediaType: MediaType) extends Response.Body.Strict[A]:
    //   // TODO decode depending on media type!
    //   override def encode(a: A): (Http.Headers, Http.Payload.Strict) =
    //     encodeWithOptionalContentType(a).leftMap: headers =>
    //       if headers.exists { case (key, _) => key === ci"Content-Type" }
    //       then headers
    //       else (ci"Content-Type" -> mediaType.print) +: headers
    //   protected def encodeWithOptionalContentType(a: A): (Http.Headers, Http.Payload.Strict)

    // val Empty: Response.Body.Strict.Empty[Unit] = new Empty[Unit]:
    //   override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Unit] = ().valid
    //   override def encode(a: Unit): (Http.Headers, Http.Payload.Strict) =
    //     (Chain.empty, Http.Payload.Strict(Array.emptyByteArray))

    // val Binary: Response.Body.Strict.Payload[Array[Byte]] =
    //   new Payload[Array[Byte]](string.format("binary"), MediaType.application.octetStream):
    //     override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Array[Byte]] =
    //       payload.valid
    //     override def encodeWithOptionalContentType(a: Array[Byte]): (Http.Headers, Payload.Strict) =
    //       (Chain.empty, Http.Payload.Strict(a))

    // def apply[A](
    //     f: (Http.Headers, Array[Byte]) => Codec.Result[Data],
    //     g: Data => (Http.Headers, Array[Byte]),
    //     of: Codec[?, ?, A],
    //     mediaType: MediaType
    // ): Response.Body.Strict.Payload[A] = new Payload[A](of, mediaType):
    //   override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A] =
    //     f(headers, payload).andThen(of.decode)
    //   override def encodeWithOptionalContentType(a: A): (Http.Headers, Payload.Strict) =
    //     g(of.encode(a)).map(Http.Payload.Strict.apply)
