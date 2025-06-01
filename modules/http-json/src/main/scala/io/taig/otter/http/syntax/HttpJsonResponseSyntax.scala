package io.taig.otter.http.syntax

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.otter.http.syntax.CodeSyntax.*
import io.taig.otter.http.syntax.HttpJsonBodySyntax.*
import io.taig.otter.http.syntax.ResultSyntax.*
import io.taig.otter.syntax.EnrichedSyntax.*

trait HttpJsonResponseSyntax:
  def response[A](results: Results[Json, A]): Response[Json, A] = ResponseSyntax.response(
    results,
    validation = result(
      code.unprocessableEntity,
      body(error("validation", field("violations", violations).toRecord).name("ValidationViolation"))
    ),
    failure = result(code.internalServerError, body(string.nullable))
  )

  def response[A](result: Result[Json, A]): Response[Json, A] =
    response(results = result.toResults)

object HttpJsonResponseSyntax extends HttpJsonResponseSyntax
