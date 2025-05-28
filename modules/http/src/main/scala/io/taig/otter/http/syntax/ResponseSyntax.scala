package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Violations
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.http.Results

trait ResponseSyntax:
  def response[S[_], A](
      results: Results[S, A],
      validation: Result[S, Violations],
      failure: Result[S, Option[String]]
  ): Response[S, A] = Response(value = Response.Value(results, validation, failure), metadata = Metadata.Empty)

object ResponseSyntax extends ResponseSyntax
