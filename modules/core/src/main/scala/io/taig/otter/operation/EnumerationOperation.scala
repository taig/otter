package io.taig.otter.operation

import io.taig.enumeration.ext.Mapping
import io.taig.otter.OperationInvariant

trait EnumerationOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def enumeration[Value[a] <: Shape[a], A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[Value, B]

object EnumerationOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: EnumerationOperation[Shape, Self]
  ): EnumerationOperation[Shape, Self] = operation

  given OperationInvariant[EnumerationOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: EnumerationOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): EnumerationOperation[Shape, T] = new EnumerationOperation[Shape, T]:
        override def enumeration[Value[a] <: Shape[a], A, B](schema: => Value[A], mapping: Mapping[B, A]): T[Value, B] =
          fK(operation.enumeration(schema, mapping))
