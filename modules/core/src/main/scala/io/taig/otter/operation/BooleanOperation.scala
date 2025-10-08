package io.taig.otter.operation

import io.taig.otter.OperationInvariant

trait BooleanOperation[+Self[_]]:
  self =>

  def boolean: Self[Boolean]

object BooleanOperation:
  inline def apply[Self[_]](using operation: BooleanOperation[Self]): BooleanOperation[Self] = operation

  given OperationInvariant[[Shape[_], Self[_[a] <: Shape[a], _]] =>> BooleanOperation[[a] =>> Self[Nothing, a]]] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: BooleanOperation[[a] =>> Self[Nothing, a]])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): BooleanOperation[[a] =>> T[Nothing, a]] = new BooleanOperation[[a] =>> T[Nothing, a]]:
        override def boolean: T[Nothing, Boolean] = fK(operation.boolean)
