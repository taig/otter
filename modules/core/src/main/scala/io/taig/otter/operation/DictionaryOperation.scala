package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.otter.OperationInvariant

trait DictionaryOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def dictionary[Value[a] <: Shape[a], A](
      schema: => Value[A],
      validation: Validation[Constraint.Object, A]
  ): Self[Value, List[(String, A)]]

object DictionaryOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: DictionaryOperation[Shape, Self]
  ): DictionaryOperation[Shape, Self] = operation

  given OperationInvariant[DictionaryOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: DictionaryOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): DictionaryOperation[Shape, T] = new DictionaryOperation[Shape, T]:
        override def dictionary[Value[a] <: Shape[a], A](
            schema: => Value[A],
            validation: Validation[Constraint.Object, A]
        ): T[Value, List[(String, A)]] = fK(operation.dictionary(schema, validation))
