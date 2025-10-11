package io.taig.otter.operation

import io.taig.otter.OperationK

trait TupleOperation[-Shape[_], Self[_[a] <: Shape[a], _]]
    extends EmptyOperation[Self],
      LiftOperation[Shape, Self],
      ZipOperation[Shape, Self]

object TupleOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: TupleOperation[Shape, Self]
  ): TupleOperation[Shape, Self] = operation

  given OperationK[TupleOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: TupleOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): TupleOperation[Shape, T] = new TupleOperation[Shape, T]:
        override def empty: T[Nothing, Unit] = fK(operation.empty)

        override def lift[Value[a] <: Shape[a], A](value: => Value[A]): T[Value, A] = fK(operation.lift(value))

        extension [S[a] <: Shape[a], A](self: T[S, A])
          override def zip[G[a] <: Shape[a], B](schema: T[G, B]): T[[a] =>> S[a] | G[a], (A, B)] =
            fK(operation.zip(gK(self))(gK(schema)))
