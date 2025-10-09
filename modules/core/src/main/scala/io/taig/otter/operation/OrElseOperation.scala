package io.taig.otter.operation

trait OrElseOperation[-Shape[_], Self[_[a] <: Shape[a], _]]:
  extension [S[a] <: Shape[a], A](self: Self[S, A])
    def orElse[S1[a] >: S[a] <: Shape[a], B](schema: Self[S1, B]): Self[S1, Either[A, B]]
