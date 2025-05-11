package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.Invariant

sealed abstract class Result[+S[_], A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Result[S, B] = Result.Modify(self = this, f, g)

  final def orElse[T[_], B](result: Result[T, B]): Result[S + T, Either[A, B]] =
    Result.OrElse(left = this, right = result)

object Result:
  final private[otter] case class Modify[S[_], A, B](self: Result[S, A], f: A => B, g: B => A) extends Result[S, B]

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Result[S, A], right: Result[T, B])
      extends Result[S + T, Either[A, B]]

  final private[otter] case class Payload[S[_], A, B](self: Result.Root[A], bodies: Bodies[S, B])
      extends Result[S, (A, B)]

  final private[otter] case class Root[A](code: Code, headers: Headers[A]) extends Result[Nothing, A]

  given [S[_]]: Invariant.Coproduct[Result[S, *], Result[S, *]] with
    override def result: Invariant[Result[S, *]] = this

    extension [A](self: Result[S, A])
      override def orElse[B](codec: Result[S, B]): Result[S, Either[A, B]] =
        self.orElse(codec)
      override def imap[B](f: A => B)(g: B => A): Result[S, B] = self.imap(f)(g)
