package io.taig.otter.http.syntax

import io.taig.otter.Enrichment
import io.taig.otter.Merge
import io.taig.otter.http.*

import scala.annotation.targetName

trait ResultSyntax:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      bodies: Bodies[S, B]
  )(using merge: Merge[A, B]): Result[S, merge.Out] =
    Result(Enrichment(Result.Value.Payload(Result.Value.Root(code, headers), bodies))).merge

  def result[S[_], A](code: Code, bodies: Bodies[S, A]): Result[S, A] = Result(
    Enrichment(
      Result.Value
        .Payload(Result.Value.Root(code, headers = Headers.Empty), bodies)
        .imap((_, a) => a)(a => ((), a))
    )
  )

  @targetName("a")
  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] = result(code, bodies = body.toBodies)

  def result[S[_], A](code: Code): Result[Nothing, Unit] = Result(
    Enrichment(Result.Value.Root(code, headers = Headers.Empty))
  )

object ResultSyntax extends ResultSyntax
