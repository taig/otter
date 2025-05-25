package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Metadata
import io.taig.otter.schema.Schema

sealed abstract class Result[+S[_], A] extends Product with Serializable:
  def code: Code
  def bodies: Option[Bodies[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Result[S, B] = Result.Modify(self = this, f, g)

  final def toResults: Results[S, A] = Results.Root(result = this)

object Result:
  final private[otter] case class Modify[S[_], A, B](self: Result[S, A], f: A => B, g: B => A) extends Result[S, B]:
    export self.{bodies, code}

  final private[otter] case class Payload[S[_], A, B](self: Result.Root[A], payload: Bodies[S, B])
      extends Result[S, (A, B)]:
    export self.{code}
    override def bodies: Option[Bodies[S, ?]] = payload.some

  final private[otter] case class Root[A](code: Code, headers: Headers[A])
      extends Result[Nothing, A]:
    override def bodies: Option[Bodies[Nothing, ?]] = none

  given [S[_]]: Schema[Result[S, *]] with
    override def imap[A, B](fa: Result[S, A])(f: A => B)(g: B => A): Result[S, B] = fa.imap(f)(g)
