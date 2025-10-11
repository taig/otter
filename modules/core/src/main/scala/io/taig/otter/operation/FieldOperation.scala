package io.taig.otter.operation

import io.taig.otter.OperationK

trait FieldOperation[-Shape[_], Self[_[a] <: Shape[a], _]]:
  def apply[Value[a] <: Shape[a], A](name: String, value: => Value[A]): Self[Value, A]

  def optional[Value[a] <: Shape[a], A](self: Self[Value, A]): Self[Value, Option[A]]

  def optional[Value[a] <: Shape[a], A](self: Self[Value, A], default: => A): Self[Value, A]

object FieldOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: FieldOperation[Shape, Self]
  ): FieldOperation[Shape, Self] = operation

  given OperationK[FieldOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: FieldOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): FieldOperation[Shape, T] = new FieldOperation[Shape, T]:
        override def apply[Value[a] <: Shape[a], A](name: String, value: => Value[A]): T[Value, A] =
          fK(operation.apply(name, value))

        override def optional[Value[a] <: Shape[a], A](self: T[Value, A]): T[Value, Option[A]] =
          fK(operation.optional(gK(self)))

        override def optional[Value[a] <: Shape[a], A](self: T[Value, A], default: => A): T[Value, A] =
          fK(operation.optional(gK(self), default))
