package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.+
import io.taig.otter.operation.*
import io.taig.otter.Reference
import io.taig.otter.Enrichment

type Results[+S[_], A] = Enrichment[Results.Value[S, *], A]

object Results:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def toChain: Chain[Reference[S, ?]]

    final def imap[B](f: A => B)(g: B => A): Results.Value[S, B] = Results.Value.Modify(self = this, f, g)

    final def orElse[T[_], B](results: Results.Value[T, B]): Results.Value[S + T, Either[A, B]] =
      Results.Value.OrElse(left = this, right = results)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Results.Value[S, A], f: A => B, g: B => A)
        extends Results.Value[S, B]:
      export self.toChain

    final private[otter] case class OrElse[S[_], T[_], A, B](left: Results.Value[S, A], right: Results.Value[T, B])
        extends Results.Value[S + T, Either[A, B]]:
      override def toChain: Chain[Reference[S + T, ?]] = left.toChain ++ right.toChain

    final private[otter] case class Root[S[_], A](result: Reference[S, A]) extends Results.Value[S, A]:
      override def toChain: Chain[Reference[S, ?]] = Chain.one(result)
