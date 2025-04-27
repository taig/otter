package io.taig.otter.http
import io.taig.otter.Merge

trait ResultDsl:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      body: Body[S, B]
  )(using merge: Merge[A, B]): Result[S, merge.Out] = Result.Root(code, headers, body).merge

  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] =
    Result.Root(code, headers = Headers.Empty, body).imap((_, a) => a)(a => ((), a))

  def result[S[_], A](code: Code): Result[Nothing, Unit] =
    Result.Root(code, headers = Headers.Empty, body = Body.Empty).imap(_ => ())(_ => ((), ()))

object ResultDsl extends ResultDsl
