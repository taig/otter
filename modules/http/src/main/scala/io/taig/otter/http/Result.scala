package io.taig.otter.http

import io.taig.otter.http.header.MediaType
import io.taig.otter.Reference

sealed abstract class Result[+S, A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Result[S, B] = Result.Modify(self = this, f, g)

  final def orElse[T, B](result: Result[T, B]): Result[S | T, Either[A, B]] = Result.OrElse(left = this, right = result)

object Result:
  final private[otter] case class Modify[S, A, B](self: Result[S, A], f: A => B, g: B => A) extends Result[S, B]

  final private[otter] case class OrElse[S, T, A, B](left: Result[S, A], right: Result[T, B])
      extends Result[S | T, Either[A, B]]

  final private[otter] case class Root[S[_], A, B](
      code: Code,
      mediaType: MediaType,
      headers: Headers[A],
      codec: Reference[S, B]
  ) extends Result[S[A], (A, B)]
