package io.taig.otter.http.syntax

import io.taig.otter.Merge
import io.taig.otter.http.Bodies
import io.taig.otter.http.HttpExport.*
import io.taig.otter.http.Code
import io.taig.otter.http.Headers
import io.taig.otter.http.Result

trait ResultSyntax:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      bodies: Bodies[S, B]
  )(using merge: Merge[A, B]): Result[S, merge.Out] =
    Result.Payload(Result.Root(code, headers), bodies).merge

  def result[S[_], A](code: Code, bodies: Bodies[S, A]): Result[S, A] = Result
      .Payload(Result.Root(code, headers = Headers.Empty), bodies)
      .imap((_, a) => a)(a => ((), a))

  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] = result(code, bodies = ???)

  def result[S[_], A](code: Code): Result[Nothing, Unit] = Result.Root(code, headers = Headers.Empty)

object ResultSyntax extends ResultSyntax
