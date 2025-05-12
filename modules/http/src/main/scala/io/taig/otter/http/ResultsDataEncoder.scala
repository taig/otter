package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.HttpError.ContentNegotiationFailed

final class ResultsDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val writer = ResultDataEncoder(encoder)

  def apply[A](
      results: Results[S, A],
      accept: Option[Accept],
      a: A
  ): Either[ContentNegotiationFailed, Response.Data] = results match
    case Results.Modify(self, _, g)  => apply(results = self, accept, g(a))
    case Results.OrElse(left, right) => a.fold(apply(results = left, accept, _), apply(results = right, accept, _))
    case Results.Root(result)        => writer(result, accept, a)
