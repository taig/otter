package io.taig.otter.operation

import io.taig.otter.OperationK

trait NullableOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def nullable[Value[a] <: Shape[a], A](value: => Value[A]): Self[Value, Option[A]]

  def nullable[Value[a] <: Shape[a], A](value: => Value[A], default: => A): Self[Value, A]

object NullableOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: NullableOperation[Shape, Self]
  ): NullableOperation[Shape, Self] = operation

  given OperationK[NullableOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: NullableOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): NullableOperation[Shape, T] = new NullableOperation[Shape, T]:
        override def nullable[Value[a] <: Shape[a], A](value: => Value[A]): T[Value, Option[A]] =
          fK(operation.nullable(value))

        override def nullable[Value[a] <: Shape[a], A](value: => Value[A], default: => A): T[Value, A] =
          fK(operation.nullable(value, default))
