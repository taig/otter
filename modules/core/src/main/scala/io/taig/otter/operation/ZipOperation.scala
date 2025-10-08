package io.taig.otter.operation

trait ZipOperation[-Shape[_], Self[_[a] <: Shape[a], _]]:
  extension [S[a] <: Shape[a], A](self: Self[S, A])
    def zip[T[a] <: Shape[a], B](schema: Self[T, B]): Self[[a] =>> S[a] | T[a], (A, B)]
