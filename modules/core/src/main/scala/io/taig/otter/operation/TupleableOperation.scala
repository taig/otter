package io.taig.otter.operation

import io.taig.otter.Reference

trait TupleableOperation[Tuple[_[a] <: Bound[a], _], Bound[_]]:
  extension [S[a] <: Bound[a], A](self: S[A]) def toTuple: Tuple[S, A]

object TupleableOperation:
  inline def apply[Tuple[_[a] <: Bound[a], _], Bound[_]](using
      self: TupleableOperation[Tuple, Bound]
  ): TupleableOperation[Tuple, Bound] =
    self

  def derived[Tuple[_[a] <: Bound[a], _] <: Matchable, Bound[_]](using
      TupleOperation[Tuple, Bound]
  ): TupleableOperation[Tuple, Bound] = new TupleableOperation[Tuple, Bound]:
    extension [S[a] <: Bound[a], A](self: S[A])
      override def toTuple: Tuple[S, A] = TupleOperation[Tuple, Bound].lift(schema = Reference.now(self))
