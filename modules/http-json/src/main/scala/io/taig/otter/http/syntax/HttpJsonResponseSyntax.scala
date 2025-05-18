package io.taig.otter.http.syntax

import io.taig.otter.http.Results
import io.taig.otter.http.syntax.ResultSyntax.*
import io.taig.otter.http.syntax.HttpJsonBodySyntax.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.http.syntax.CodeSyntax.*
import io.taig.otter.http.Response
import io.taig.otter.Json
import io.taig.otter.http.Result

trait HttpJsonResponseSyntax:
  def response[S[_], A](results: Results[S, A]): Response[S, Json, A] = ResponseSyntax.response(
    results,
    validation = result(
      code.unprocessableEntity,
      json(error("validation", field("violations", violations).toRecord))
    ),
    failure = result(code.internalServerError, json(string.nullable))
  )

  def response[S[_], A](result: Result[S, A]): Response[S, Json, A] =
    response(results = result.toResults)

object HttpJsonResponseSyntax extends HttpJsonResponseSyntax
