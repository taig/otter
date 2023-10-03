package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.{Codec, Data}
import io.taig.otter.codecs.string
import io.taig.otter.http.Http.Payload
import io.taig.otter.validation.{History, Violation, Violations}

final case class Response[A](results: Results[A], violations: Result[Violations]):
  def results[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Validated[Violations, A] = results.decode(response)
  def encode(a: Validated[Violations, A]): Http.Response = a.fold(violations.encode, results.encode)

object Response:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }

    def codec: Option[Codec[?]]

    def mediaType: Option[MediaType]

    def decode(headers: Http.Headers, payload: Http.Payload): Validated[Violations, A]
    def encode(a: A): (Http.Headers, Http.Payload)

  object Body extends ToResponseBodyOps:
    sealed abstract class Strict[A](val codec: Option[Codec[?]], val mediaType: Option[MediaType])
        extends Response.Body[A]:
      self =>
      final override type Self[a] = Response.Body.Strict[a]

      override def decode(headers: Http.Headers, payload: Payload): Validated[Violations, A] = payload match
        case Http.Payload.Strict(data) => decode(headers, data)
        case Http.Payload.Streaming(_) =>
          Violations.oneNec(History.Root / "body", Violation.tpe("strict", "streaming")).invalid
      def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A]
      override def encode(a: A): (Http.Headers, Http.Payload.Strict)

    object Strict:
      val Empty: Response.Body.Strict[Unit] = new Strict[Unit](None, None):
        override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Unit] = ().valid
        override def encode(a: Unit): (Http.Headers, Http.Payload.Strict) =
          (Chain.empty, Http.Payload.Strict(Array.emptyByteArray))

      val Binary: Response.Body.Strict[Array[Byte]] =
        new Strict[Array[Byte]](Some(string.format("binary")), Some(MediaType.application.octetStream)):
          override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, Array[Byte]] =
            payload.valid
          override def encode(a: Array[Byte]): (Http.Headers, Http.Payload.Strict) =
            (Chain.empty, Http.Payload.Strict(a))

      def apply[A](
          f: (Http.Headers, Array[Byte]) => Validated[Violations, Data],
          g: Data => (Http.Headers, Array[Byte]),
          of: Codec[A],
          mediaType: MediaType
      ): Response.Body.Strict[A] = new Strict[A](Some(of), Some(mediaType)):
        override def decode(headers: Http.Headers, payload: Array[Byte]): Validated[Violations, A] =
          f(headers, payload).andThen(of.decode)
        override def encode(a: A): (Http.Headers, Http.Payload.Strict) = g(of.encode(a)).map(Http.Payload.Strict.apply)
