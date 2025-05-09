package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.StacktracePrinter
import io.taig.otter.+
import io.taig.otter.http.header.Accept
import org.typelevel.ci.*
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.CodeDsl.*
import io.taig.otter.Violation
import cats.data.Chain
import io.taig.otter.http.Headers.Data.accept

final class ResponseDataEncoder[S[_], T[_]](encoder: PayloadEncoder[S + T], debug: Boolean):
  val payload = ResultDataEncoder(encoder)

  def apply[A](
      response: Response[S, T, A],
      headers: Headers.Data,
      result: Either[Throwable, Validated[Request.Error, A]]
  ): Response.Data = headers.accept
    .leftMap("header" /: _)
    .leftMap(Request.Error.ValidationViolations.apply)
    .fold(
      error => apply(response, accept = none, result = error.invalid.asRight),
      apply(response, _, result)
    )

  def apply[A](
      response: Response[S, T, A],
      accept: Option[Accept],
      result: Either[Throwable, Validated[Request.Error, A]]
  ): Response.Data = result
    .match
      case Left(throwable) =>
        payload(result = response.failure, accept, Option.when(debug)(StacktracePrinter(throwable)))
      case Right(Validated.Invalid(Request.Error.MediaTypeUnsupported)) =>
        Response.Data(code = unsupportedMediaTypes, headers = Chain.empty, body = Array.emptyByteArray).some
      case Right(Validated.Invalid(Request.Error.ValidationViolations(violations))) =>
        payload(result = response.validation, accept, violations)
      case Right(Validated.Valid(a)) => payload(result = response.result, accept, a)
    .getOrElse(Response.Data(code = notAcceptable, headers = Chain.empty, body = Array.emptyByteArray))
