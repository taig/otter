package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.validation.Validation
import cats.data.Chain
import io.taig.otter.InvariantK

trait DictionaryOperation[Self[_], -Value[_]]:
  def dictionary[A](
      schema: => Value[A],
      validation: Validation[Constraint.Object, A]
  ): Self[List[(String, A)]]

  def constraints[A](self: Self[A]): Chain[Constraint.Object]

object DictionaryOperation:
  inline def apply[Self[_], Value[_]](using
      operation: DictionaryOperation[Self, Value]
  ): DictionaryOperation[Self, Value] = operation
