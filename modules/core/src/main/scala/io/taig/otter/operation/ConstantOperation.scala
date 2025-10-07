package io.taig.otter.operation

import cats.Eq
import io.taig.otter.FunctorK

trait ConstantOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  self =>

  def constant[Value[a] <: Shape[a], A: Eq](schema: => Value[A], value: A): Self[Value, A]

  def mapK[T[_[_], _]](fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]): ConstantOperation[Shape, T] =
    new ConstantOperation[Shape, T]:
      override def constant[Value[a] <: Shape[a], A: Eq](schema: => Value[A], value: A): T[Value, A] =
        fK(self.constant(schema, value))

object ConstantOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: ConstantOperation[Shape, Self]
  ): ConstantOperation[Shape, Self] =
    operation

  // given [Shape[_]]: FunctorK[[s[_[a] <: Shape[a], _]] =>> ConstantOperation[Shape, s]] = ???
