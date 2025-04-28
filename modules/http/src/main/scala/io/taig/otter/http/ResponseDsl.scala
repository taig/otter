package io.taig.otter.http

import io.taig.otter.http.ResultDsl.*
import io.taig.otter.http.CodeDsl.*
import cats.syntax.all.*

trait ResponseDsl:
  def response[S[_], T[_], A](
      result: Result[S, A],
      errors: Result[T, Response.Error],
      failure: Result[T, Option[String]]
  ): Response[S, T, A] = Response(result, errors, failure)

object ResponseDsl extends ResponseDsl
