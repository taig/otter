package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.+
import io.taig.otter.operation.*
import io.taig.otter.Reference

sealed abstract class Results[+S[_], A] extends Product with Serializable:
  def toChain: Chain[Reference[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Results[S, B] = Results.Modify(self = this, f, g)

  final def orElse[T[_], B](results: Results[T, B]): Results[S + T, Either[A, B]] =
    Results.OrElse(left = this, right = results)

object Results:
  final private[otter] case class Modify[S[_], A, B](self: Results[S, A], f: A => B, g: B => A) extends Results[S, B]:
    export self.toChain

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Results[S, A], right: Results[T, B])
      extends Results[S + T, Either[A, B]]:
    override def toChain: Chain[Reference[S + T, ?]] = left.toChain ++ right.toChain

  final private[otter] case class Root[S[_], A](result: Reference[S, A]) extends Results[S, A]:
    override def toChain: Chain[Reference[S, ?]] = Chain.one(result)
