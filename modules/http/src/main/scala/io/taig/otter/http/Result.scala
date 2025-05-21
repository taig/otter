package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Metadata
import io.taig.otter.schema.Schema

sealed abstract class Result[+S[_], A] extends Product with Serializable:
  def code: Code
  def bodies: Option[Bodies[S, ?]]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Result[S, A]

  final def imap[B](f: A => B)(g: B => A): Result[S, B] = Result.Modify(self = this, f, g)

  final def orElse[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] =
    toResults.orElse(result)

  final def :+[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] = orElse(result)

  final def +:[T[_], B](result: Result[T, B]): Results[S + T, Either[B, A]] = result.orElse(this)

  final def toResults: Results[S, A] = Results.Root(result = this)

object Result:
  final private[otter] case class Modify[S[_], A, B](self: Result[S, A], f: A => B, g: B => A) extends Result[S, B]:
    export self.{bodies, code, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Result[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Payload[S[_], A, B](self: Result.Root[A], payload: Bodies[S, B])
      extends Result[S, (A, B)]:
    export self.{code, metadata}
    override def bodies: Option[Bodies[S, ?]] = payload.some
    override def modifyMetadata(f: Metadata => Metadata): Result[S, (A, B)] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A](code: Code, headers: Headers[A], metadata: Metadata)
      extends Result[Nothing, A]:
    override def bodies: Option[Bodies[Nothing, ?]] = none
    override def modifyMetadata(f: Metadata => Metadata): Result.Root[A] = copy(metadata = f(metadata))

  given [S[_]]: Schema[Result[S, *]] with
    override def imap[A, B](fa: Result[S, A])(f: A => B)(g: B => A): Result[S, B] = fa.imap(f)(g)
