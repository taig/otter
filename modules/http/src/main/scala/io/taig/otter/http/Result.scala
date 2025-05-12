package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.Invariant
import io.taig.otter.Metadata

sealed abstract class Result[+S[_], A] extends Product with Serializable:
  def code: Code

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Result[S, A]

  final def imap[B](f: A => B)(g: B => A): Result[S, B] = Result.Modify(self = this, f, g)

  final def orElse[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] =
    toResults.orElse(result)

  final def toResults: Results[S, A] = Results.Root(result = this)

object Result:
  final private[otter] case class Modify[S[_], A, B](self: Result[S, A], f: A => B, g: B => A) extends Result[S, B]:
    export self.{code, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Result[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Payload[S[_], A, B](self: Result.Root[A], bodies: Bodies[S, B])
      extends Result[S, (A, B)]:
    export self.{code, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Result[S, (A, B)] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A](code: Code, headers: Headers[A], metadata: Metadata)
      extends Result[Nothing, A]:
    override def modifyMetadata(f: Metadata => Metadata): Result.Root[A] = copy(metadata = f(metadata))

  given [S[_]]: Invariant.Coproduct[Result[S, *], Results[S, *]] with
    override def result: Invariant[Results[S, *]] = Results.invariant
    extension [A](self: Result[S, A])
      override def imap[B](f: A => B)(g: B => A): Result[S, B] = self.imap(f)(g)
      override def orElse[B](codec: Result[S, B]): Results[S, Either[A, B]] = self.orElse(codec)
