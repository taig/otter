package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.Metadata
import io.taig.otter.Enrichment

final case class Result[+S[_], A](self: Enrichment[Result.Value[S, A]]) extends AnyVal:
  inline def value: Result.Value[S, A] = self.self

  def code: Code = value.code
  def bodies: Option[Bodies[S, ?]] = value.bodies

  def :+[T[_], B](schema: Result[T, B]): Results[S + T, Either[A, B]] = toResults :+ schema

  def +:[T[_], B](schema: Result[T, B]): Results[S + T, Either[B, A]] = schema.toResults :+ this

  def toResults: Results[S, A] = Results(Enrichment(Results.Value.Root(this)))

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

  extension [S[_], A <: Matchable](self: Result[S, A])
    inline def |[T[_], B <: Matchable](schema: Result[T, B]): Results[S + T, A | B] = self.toResults | schema

  given [S[_]]: EnrichedSchemaInvariant[Result[S, *]] with
    override def imap[A, B](fa: Result[S, A])(f: A => B)(g: B => A): Result[S, B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Result[S, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Result[S, A] =
        self.copy(self = self.self.modifyMetadata(f))
