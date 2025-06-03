package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.HttpError.ContentNegotiationFailed
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.http.header.Accept
import org.typelevel.ci.*

final class ResultDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val bodies = BodiesEncoder(encoder)

  def encode[A](
      schema: Result[S, A],
      accept: Option[Accept],
      a: A
  ): Either[ContentNegotiationFailed, Response.Data] = encode(schema = schema.value, accept, a)

  def encode[A](
      schema: Result.Value[S, A],
      accept: Option[Accept],
      a: A
  ): Either[ContentNegotiationFailed, Response.Data] = schema match
    case Result.Value.Modify(self, _, g) => encode(schema = self, accept, g(a))
    case Result.Value.Payload(self, bodies) =>
      val mediaRanges = accept.flatMap(_.toResult.right).fold(Nil)(_.toList)

      this.bodies
        .encode(bodies, accept = mediaRanges, a._2)
        .map: (mediaType, bytes) =>
          encode(schema = self, a._1)
            .modifyHeaders((ci"Content-Type", mediaType.show) +: _)
            .withBody(bytes)
    case result: Result.Value.Root[?] => encode(result, a).asRight

  def encode[A](schema: Result[S, A], a: A): Response.Data = encode(schema = schema.value, a)

  def encode[A](schema: Result.Value[S, A], a: A): Response.Data = schema match
    case Result.Value.Modify(self, _, g) => encode(schema = self, g(a))
    case Result.Value.Payload(self, bodies) =>
      val (mediaType, bytes) = this.bodies.encode(bodies, a._2)
      encode(schema = self, a._1)
        .modifyHeaders((ci"Content-Type", mediaType.show) +: _)
        .withBody(bytes)
    case result: Result.Value.Root[?] => encode(result, a)

  def encode[A](schema: Result.Value.Root[A], a: A): Response.Data =
    Response.Data(
      code = schema.code,
      headers = HeadersDataEncoder.encode(headers = schema.headers, a),
      body = Array.emptyByteArray
    )
