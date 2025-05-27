package io.taig.otter.http.syntax

import io.taig.otter.Merge
import io.taig.otter.Metadata
import io.taig.otter.http.*

trait ResultSyntax:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      bodies: Bodies[S, B]
  )(using merge: Merge[A, B]): Result[S, merge.Out] = Result(
    value = Result.Value.Payload(Result.Value.Root(code, headers), bodies),
    metadata = Metadata.Empty
  ).merge

  def result[S[_], A](code: Code, bodies: Bodies[S, A]): Result[S, A] = Result(
    value = Result.Value
      .Payload(Result.Value.Root(code, headers = Headers.Empty), bodies)
      .imap((_, a) => a)(a => ((), a)),
    metadata = Metadata.Empty
  )

  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] = result(code, bodies = body.toBodies)

  def result[S[_], A](code: Code): Result[Nothing, Unit] = Result(
    value = Result.Value.Root(code, headers = Headers.Empty),
    metadata = Metadata.Empty
  )

object ResultSyntax extends ResultSyntax
