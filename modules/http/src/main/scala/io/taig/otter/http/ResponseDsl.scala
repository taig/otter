package io.taig.otter.http
import io.taig.otter.Violations

trait ResponseDsl:
  def response[S[_], T[_], A](
      result: Results[S, A],
      validation: Results[T, Violations],
      failure: Results[T, Option[String]]
  ): Response[S, T, A] = Response(result, validation, failure)

object ResponseDsl extends ResponseDsl
