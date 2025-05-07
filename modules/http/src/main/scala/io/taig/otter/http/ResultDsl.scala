package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Merge

trait ResultDsl:
  def result[S[_], A, B](
      code: Code,
      headers: Headers[A],
      bodies: Option[Bodies[S, B]]
  )(using merge: Merge[A, B]): Result[S, merge.Out] = Result.Root(code, headers, bodies).merge

  def result[S[_], A](code: Code, bodies: Option[Bodies[S, A]]): Result[S, A] =
    Result.Root(code, headers = Headers.Empty, bodies).imap((_, a) => a)(a => ((), a))

  def result[S[_], A](code: Code, bodies: Bodies[S, A]): Result[S, A] = result(code, bodies = bodies.some)

  def result[S[_], A](code: Code, body: Body[S, A]): Result[S, A] = result(code, bodies = body.toBodies)

  def result[S[_], A](code: Code): Result[Nothing, Unit] =
    Result.Root(code, headers = Headers.Empty, bodies = none).imap(_ => ())(_ => ((), ()))

object ResultDsl extends ResultDsl
