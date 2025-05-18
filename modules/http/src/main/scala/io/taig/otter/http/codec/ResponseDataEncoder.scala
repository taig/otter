package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.StacktracePrinter
import io.taig.otter.Violations
import io.taig.otter.http.Headers.Data.accept
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.syntax.CodeSyntax.*
import io.taig.otter.http.Response
import io.taig.otter.http.Headers

final class ResponseDataEncoder[S[_], T[_]](encoder: PayloadEncoder[S + T], debug: Boolean):
  val results = ResultsDataEncoder(encoder)

  def apply[A](
      response: Response[S, T, A],
      headers: Headers.Data,
      result: Either[Failure | MediaTypeUnsupported | ValidationViolations, A]
  ): Response.Data = headers.accept
    .leftMap("header" /: _)
    .leftMap(ValidationViolations.apply)
    .fold(
      error => apply(response, accept = none, result = error.asLeft),
      apply(response, _, result)
    )

  def apply[A](
      response: Response[S, T, A],
      accept: Option[Accept],
      result: Either[Failure | MediaTypeUnsupported | ValidationViolations, A]
  ): Response.Data = result
    .match
      case Right(a) => results.encode(schema = response.results, accept, a)
      case Left(Failure(throwable)) =>
        results.result.encode(schema = response.failure, accept, Option.when(debug)(StacktracePrinter(throwable)))
      case Left(MediaTypeUnsupported) =>
        Response.Data(code = unsupportedMediaTypes, headers = Chain.empty, body = Array.emptyByteArray).asRight
      case Left(ValidationViolations(violations)) =>
        results.result.encode(schema = response.validation, accept, violations)
    .getOrElse(Response.Data(code = notAcceptable, headers = Chain.empty, body = Array.emptyByteArray))
