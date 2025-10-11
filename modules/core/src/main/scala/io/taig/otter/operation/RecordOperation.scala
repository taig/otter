package io.taig.otter.operation

import io.taig.otter.OperationK

trait RecordOperation[-Shape[_], Self[_[a] <: Shape[a], _]]
    extends EmptyOperation[Self],
      LiftOperation[Shape, Self],
      ZipOperation[Shape, Self]

object RecordOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: RecordOperation[Shape, Self]
  ): RecordOperation[Shape, Self] = operation

  given OperationK[RecordOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: RecordOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): RecordOperation[Shape, T] = new RecordOperation[Shape, T]:
        override def empty: T[Nothing, Unit] = fK(operation.empty)

        override def lift[Value[a] <: Shape[a], A](value: => Value[A]): T[Value, A] = fK(operation.lift(value))

        extension [S[a] <: Shape[a], A](self: T[S, A])
          override def zip[H[a] <: Shape[a], B](schema: T[H, B]): T[[a] =>> S[a] | H[a], (A, B)] =
            fK(operation.zip(gK(self))(gK(schema)))
