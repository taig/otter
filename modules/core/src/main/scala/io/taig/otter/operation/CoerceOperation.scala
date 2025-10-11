package io.taig.otter.operation

import io.taig.otter.OperationK

trait CoerceOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def coerce[Value[a] <: Shape[a], A](schema: => Value[A]): Self[Value, A]

object CoerceOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: CoerceOperation[Shape, Self]
  ): CoerceOperation[Shape, Self] = operation

  given OperationK[CoerceOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: CoerceOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): CoerceOperation[Shape, T] = new CoerceOperation[Shape, T]:
        override def coerce[Value[a] <: Shape[a], A](schema: => Value[A]): T[Value, A] =
          fK(operation.coerce(schema))
