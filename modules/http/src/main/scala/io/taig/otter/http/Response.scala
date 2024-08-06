package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec
import org.typelevel.ci.*
import io.taig.otter.Data

final case class Response[A](
    results: Results[A],
    mediaTypesUnsupported: Result[Codec.Error],
    validationViolations: Result[Codec.Error]
):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = results.decode(response)

  def encode(a: Validated[Codec.Error, A]): Http.Response =
    a.fold(validationViolations.encode, results.encode)
