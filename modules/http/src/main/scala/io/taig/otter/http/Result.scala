package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Enrichment

type Result[+S[_], A] = Enrichment[Result.Value[S, *], A]

object Result:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def code: Code
    def bodies: Option[Bodies[S, ?]]

    final def imap[B](f: A => B)(g: B => A): Result.Value[S, B] = Result.Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Result.Value[S, A], f: A => B, g: B => A)
        extends Result.Value[S, B]:
      export self.{bodies, code}

    final private[otter] case class Payload[S[_], A, B](self: Result.Value.Root[A], payload: Bodies[S, B])
        extends Result.Value[S, (A, B)]:
      export self.code
      override def bodies: Option[Bodies[S, ?]] = payload.some

    final private[otter] case class Root[A](code: Code, headers: Headers[A]) extends Result.Value[Nothing, A]:
      override def bodies: Option[Bodies[Nothing, ?]] = none
