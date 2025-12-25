package io.taig.otter.operation

import io.taig.otter.Append
import cats.Invariant

abstract class TupleableOperation[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Tuple[+_[a] <: Bound[a], a] <: Bound[a],
    TupleRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
    TupleWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
]:
  extension [F[a] <: Bound[a], A](self: Self[F, A])
    final def :*[S[+_[a] <: Bound[a], a] >: F[a] <: Bound[a], T[a] >: F[a] <: Bound[a], B](schema: => S[T, B])(using
        append: Append[A, B]
    )(using Invariant[Self[S[T, *], *]]): Self[S[T, *], append.Out] = ???
    // self.zip(apply(schema = Reference.later(schema))).imap(append.apply)(append.unapply)
