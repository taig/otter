package io.taig.otter.http

trait ResponseDsl:
  def response[S, A](result: Result[S, A]): Response[S, A] =
    Response(result, errors = ???, failure = ???)

object ResponseDsl extends ResponseDsl
