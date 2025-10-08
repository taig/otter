package io.taig.otter.operation

import cats.Eq
import io.taig.otter.FunctorK
import io.taig.otter.OperationInvariant

trait ConstantOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def constant[Value[a] <: Shape[a], A: Eq](schema: => Value[A], value: A): Self[Value, A]

object ConstantOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: ConstantOperation[Shape, Self]
  ): ConstantOperation[Shape, Self] = operation

  given OperationInvariant[ConstantOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: ConstantOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => (x: T[Value, A]) => Self[Value, A]
      ): ConstantOperation[Shape, T] = new ConstantOperation[Shape, T]:
        override def constant[Value[a] <: Shape[a], A: Eq](schema: => Value[A], value: A): T[Value, A] =
          fK(operation.constant(schema, value))
