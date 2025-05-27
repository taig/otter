package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Metadata
import io.taig.otter.operation.*

final case class Results[+S[_], A](value: Results.Value[S, A], metadata: Metadata):
  def toChain: Chain[Result[S, ?]] = value.toChain

  def orElse[T[_], B](results: Results[T, B]): Results[S + T, Either[A, B]] =
    Results(value.orElse(results.value), metadata = Metadata.Empty)

  def :+[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] =
    orElse(result.toResults)

  def +:[T[_], B](result: Result[T, B]): Results[S + T, Either[B, A]] =
    result.toResults.orElse(this)

object Results:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def toChain: Chain[Result[S, ?]]

    final def imap[B](f: A => B)(g: B => A): Results.Value[S, B] = Results.Value.Modify(self = this, f, g)

    final def orElse[T[_], B](results: Results.Value[T, B]): Results.Value[S + T, Either[A, B]] =
      Results.Value.OrElse(left = this, right = results)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Results.Value[S, A], f: A => B, g: B => A)
        extends Results.Value[S, B]:
      export self.toChain

    final private[otter] case class OrElse[S[_], T[_], A, B](left: Results.Value[S, A], right: Results.Value[T, B])
        extends Results.Value[S + T, Either[A, B]]:
      override def toChain: Chain[Result[S + T, ?]] = left.toChain ++ right.toChain

    final private[otter] case class Root[S[_], A](result: Result[S, A]) extends Results.Value[S, A]:
      override def toChain: Chain[Result[S, ?]] = Chain.one(result)

  extension [S[_], A <: Matchable](self: Results[S, A])
    inline def or[T[_], B <: Matchable](results: Results[T, B]): Results[S + T, A | B] =
      self
        .orElse(results)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    inline def |[T[_], B <: Matchable](result: Result[T, B]): Results[S + T, A | B] =
      self.or(result.toResults)

  given [S[_]]: EnrichedSchemaInvariant[Results[S, *]] with
    override def imap[A, B](fa: Results[S, A])(f: A => B)(g: B => A): Results[S, B] =
      fa.copy(value = fa.value.imap(f)(g))

    extension [A](self: Results[S, A])
      override def metadata: Metadata = self.self.metadata
      override def metadata(f: Metadata => Metadata): Results[S, A] =
        self.copy(metadata = f(self.metadata))
