package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.+
import io.taig.otter.Invariant

sealed abstract class Results[+S[_], A] extends Product with Serializable:
  def toChain: Chain[Result[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Results[S, B] = Results.Modify(self = this, f, g)

  final def orElse[T[_], B](results: Results[T, B]): Results[S + T, Either[A, B]] =
    Results.OrElse(left = this, right = results)

  final def orElse[T[_], B](result: Result[T, B]): Results[S + T, Either[A, B]] =
    orElse(results = result.toResults)

object Results:
  final private[otter] case class Modify[S[_], A, B](self: Results[S, A], f: A => B, g: B => A) extends Results[S, B]:
    export self.toChain

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Results[S, A], right: Results[T, B])
      extends Results[S + T, Either[A, B]]:
    override def toChain: Chain[Result[S + T, ?]] = left.toChain ++ right.toChain

  final private[otter] case class Root[S[_], A](result: Result[S, A]) extends Results[S, A]:
    override def toChain: Chain[Result[S, ?]] = Chain.one(result)

  given invariant[S[_]]: Invariant.Coproduct[Results[S, *], Result[S, *], Results[S, *]] with
    override def result: Invariant[Results[S, *]] = this
    override def fromElement[A](codec: Result[S, A]): Results[S, A] = codec.toResults

    extension [A](self: Results[S, A])
      override def orElse[B](codec: Results[S, B]): Results[S, Either[A, B]] =
        self.orElse(codec)
      override def imap[B](f: A => B)(g: B => A): Results[S, B] = self.imap(f)(g)
