package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.Accept

final class ResultDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val write = BodiesEncoder(encoder)

  def apply[A](
      result: Result[S, A],
      accept: Option[Accept],
      a: A
  ): Either[Response.Error.ContentNegotiationFailed, Response.Data] = result match
    case Result.Modify(self, _, g)  => apply(result = self, accept, g(a))
    case Result.OrElse(left, right) => a.fold(apply(result = left, accept, _), apply(result = right, accept, _))
    case Result.Payload(self, bodies) =>
      val mediaRanges = accept.flatMap(_.toResult.right).fold(Nil)(_.toList)
      write(bodies, accept = mediaRanges, a._2)
        .toRight(Response.Error.ContentNegotiationFailed)
        .map(apply(result = self, accept, a._1).withBody(_))
    case result @ Result.Root(_, _) => apply(result, accept, a).asRight

  def apply[A](result: Result.Root[A], accept: Option[Accept], a: A): Response.Data = Response.Data(
    code = result.code,
    headers = HeadersDataEncoder(headers = result.headers, a),
    body = Array.emptyByteArray
  )
