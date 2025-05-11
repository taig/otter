package io.taig.otter.http

import io.taig.otter.+
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.*

final class ResultDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  def apply[A](result: Result[S, A], data: Response.Data): Either[MediaTypeUnsupported | ValidationViolations, A] = result match
    case Result.Modify(self, f, _) => apply(result = self, data).map(f)
    case result @ Result.OrElse(left, right) =>
      apply(result = left, data)
      ???
    case Result.Root(code, headers) =>
      // if data.code =!= code then none
      // else HeadersDataDecoder(headers, data = data.headers).some
      ???
    case Result.Payload(self, bodies) =>
      // apply(result = self, data).map: a =>
      //   a.traverse(a => BodiesDecoder(decoder)(codec = bodies, contentType = ???, bytes = data.body).map(_.tupleLeft(a)))
      ???

object ResultDataDecoder:
  def apply[S[_], T[_], A, B](
      decoder: PayloadDecoder[S + T]
  )(result: Result.OrElse[S, T, A, B], data: Response.Data): Validated[Violations, Option[A]] = ???
