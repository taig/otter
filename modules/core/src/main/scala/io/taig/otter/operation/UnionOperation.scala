package io.taig.otter.operation

import io.taig.otter.OperationK

trait UnionOperation[-Shape[_], Self[_[a] <: Shape[a], _]]
    extends LiftOperation[Shape, Self],
      OrElseOperation[Shape, Self]

object UnionOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: UnionOperation[Shape, Self]
  ): UnionOperation[Shape, Self] = operation

  given OperationK[UnionOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: UnionOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A])(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): UnionOperation[Shape, T] = new UnionOperation[Shape, T]:
        override def lift[Value[a] <: Shape[a], A](value: => Value[A]): T[Value, A] = fK(operation.lift(value))

        extension [S[a] <: Shape[a], A](self: T[S, A])
          override def orElse[S1[a] >: S[a] <: Shape[a], B](schema: T[S1, B]): T[S1, Either[A, B]] =
            fK(operation.orElse(gK(self))(gK(schema)))
