package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.Accept

final class ResultDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val write = BodiesEncoder(encoder)

  def apply[A](result: Result[S, A], accept: Option[Accept], a: A): Option[Response.Data] = result match
    case Result.Modify(self, _, g)  => apply(result = self, accept, g(a))
    case Result.OrElse(left, right) => a.fold(apply(result = left, accept, _), apply(result = right, accept, _))
    case Result.Root(code, headers, Some(bodies)) =>
      val mediaRanges = accept.flatMap(_.toResult.right).fold(Nil)(_.toList)
      write(bodies, accept = mediaRanges, a._2).map: body =>
        Response.Data(code, headers = HeadersDataEncoder.apply(headers, a._1), body)
    case Result.Root(code, headers, None) =>
      Response
        .Data(
          code,
          headers = HeadersDataEncoder.apply(headers, a._1),
          body = Array.emptyByteArray
        )
        .some
