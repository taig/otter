package io.taig.otter.http

import cats.Invariant
import cats.data.Chain
import io.taig.otter.+

sealed abstract class Results[+S[_], A] extends Product with Serializable:
  def toChain: Chain[Result[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Results[S, B] = Results.Modify(self = this, f, g)

  final def orElse[T[_], B](results: Results[T, B]): Results[S + T, Either[A, B]] =
    Results.OrElse(left = this, right = results)

  final def :+[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] =
    orElse(results = result.toResults)

  final def +:[T[_], B](result: Result[T, B]): Results[S + T, Either[B, A]] =
    result.toResults.orElse(this)

object Results:
  final private[otter] case class Modify[S[_], A, B](self: Results[S, A], f: A => B, g: B => A) extends Results[S, B]:
    export self.toChain

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Results[S, A], right: Results[T, B])
      extends Results[S + T, Either[A, B]]:
    override def toChain: Chain[Result[S + T, ?]] = left.toChain ++ right.toChain

  final private[otter] case class Root[S[_], A](result: Result[S, A]) extends Results[S, A]:
    override def toChain: Chain[Result[S, ?]] = Chain.one(result)

  extension [S[_], A <: Matchable](self: Results[S, A])
    inline def or[T[_], B <: Matchable](results: Results[T, B]): Results[S + T, A | B] = 
      self.orElse(results).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A     => Left(a)
        case b: B     => Right(b)
      }

    inline def |[T[_], B <: Matchable](result: Result[T, B]): Results[S + T, A | B] =
      or(result.toResults)

  given [S[_]]: Invariant[Results[S, *]] with
    override def imap[A, B](fa: Results[S, A])(f: A => B)(g: B => A): Results[S, B] = fa.imap(f)(g)
