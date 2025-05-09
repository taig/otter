package io.taig.otter.http

import io.taig.otter.http.ResultDsl.*
import io.taig.otter.http.CodeDsl.*
import cats.syntax.all.*
import io.taig.otter.Violations

trait ResponseDsl:
  def response[S[_], T[_], A](
      result: Result[S, A],
      validation: Result[T, Violations],
      failure: Result[T, Option[String]]
  ): Response[S, T, A] = Response(result, validation, failure)

object ResponseDsl extends ResponseDsl
