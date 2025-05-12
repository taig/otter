package io.taig.otter.http

import io.taig.otter.Merge
import io.taig.otter.Metadata

trait ResultDsl:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      bodies: Bodies[S, B]
  )(using merge: Merge[A, B]): Result[S, merge.Out] =
    Result.Payload(Result.Root(code, headers, metadata = Metadata.Empty), bodies).merge

  def result[S[_], A](code: Code, bodies: Bodies[S, A]): Result[S, A] =
    Result
      .Payload(Result.Root(code, headers = Headers.Empty, metadata = Metadata.Empty), bodies)
      .imap((_, a) => a)(a => ((), a))

  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] = result(code, bodies = body.toBodies)

  def result[S[_], A](code: Code): Result[Nothing, Unit] =
    Result.Root(code, headers = Headers.Empty, metadata = Metadata.Empty)

object ResultDsl extends ResultDsl
