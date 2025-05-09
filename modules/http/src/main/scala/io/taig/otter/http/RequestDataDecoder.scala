package io.taig.otter.http

import io.taig.otter.Violations
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation

final class RequestDataDecoder[F[_]]:
  def apply[A](request: Request[F, A], data: Request.Data): Validated[Violations, A] =
    Validated
      .cond(
        test = data.method === request.method,
        (),
        Violations.rootNec(Violation.equal(reference = request.method.show, actual = data.method.show))
      )
      .leftMap("method" /: _) *> UrlDataDecoder(url = request.url, data = data.url)
    ???
