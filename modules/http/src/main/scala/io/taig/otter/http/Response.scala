package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Violations

final case class Response[A](
    results: Results[A],
    mediaTypesUnsupported: Result[Violations],
    validationViolations: Result[Violations]
):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode[F[_]](response: Http.Response[F]): Codec.Result[A] = ??? // results.decode(response)

  def encode[F[_]](result: Request.Result[A]): Http.Response[F] = result match
    case Request.Result.Success(a)                        => results.encode(a)
    case Request.Result.MediaTypesUnsupported(violations) => mediaTypesUnsupported.encode(violations)
    case Request.Result.ValidationViolations(violations)  => validationViolations.encode(violations)
