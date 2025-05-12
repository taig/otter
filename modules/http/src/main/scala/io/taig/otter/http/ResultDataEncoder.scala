package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.HttpError.ContentNegotiationFailed
import io.taig.otter.http.header.Accept
import org.typelevel.ci.*

final class ResultDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val writer = BodiesEncoder(encoder)

  def apply[A](
      result: Result[S, A],
      accept: Option[Accept],
      a: A
  ): Either[ContentNegotiationFailed, Response.Data] = result match
    case Result.Modify(self, _, g) => apply(result = self, accept, g(a))
    case Result.Payload(self, bodies) =>
      val mediaRanges = accept.flatMap(_.toResult.right).fold(Nil)(_.toList)

      writer(bodies, accept = mediaRanges, a._2).map: (mediaType, bytes) =>
        apply(result = self, accept, a._1)
          .modifyHeaders((ci"Content-Type", mediaType.show) +: _)
          .withBody(bytes)
    case result: Result.Root[?] => apply(result, accept, a).asRight

  def apply[A](result: Result.Root[A], accept: Option[Accept], a: A): Response.Data = Response.Data(
    code = result.code,
    headers = HeadersDataEncoder(headers = result.headers, a),
    body = Array.emptyByteArray
  )
