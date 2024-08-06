package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import org.typelevel.ci.*

final case class Response[A](
    results: Results[A],
    mediaTypesUnsupported: Result[Codec.Error],
    validationViolations: Result[Codec.Error]
):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = ??? // results.decode(response)

  def encode(result: Request.Result[A]): Http.Response = result match
    case Request.Result.Success(a)                        => results.encode(a)
    case Request.Result.MediaTypesUnsupported(violations) => mediaTypesUnsupported.encode(violations)
    case Request.Result.ValidationViolations(violations)  => validationViolations.encode(violations)
