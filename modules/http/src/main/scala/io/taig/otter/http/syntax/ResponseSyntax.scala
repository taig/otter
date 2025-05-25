package io.taig.otter.http.syntax

import io.taig.otter.Violations
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.http.Results

trait ResponseSyntax:
  def response[S[_], T[_], A](
      results: Results[S, A],
      validation: Result[T, Violations],
      failure: Result[T, Option[String]]
  ): Response[S, A] = ??? // Response(results, validation, failure)

object ResponseSyntax extends ResponseSyntax
