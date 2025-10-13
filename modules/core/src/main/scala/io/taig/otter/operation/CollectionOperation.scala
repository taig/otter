package io.taig.otter.operation

import io.taig.validation.Constraint
import io.taig.validation.Validation
import cats.data.Chain

trait CollectionOperation[Self[_], -Value[_]]:
  def indexed[A](
      schema: => Value[A],
      validation: Validation[Constraint.Collection, Vector[A]]
  ): Self[Vector[A]]

  def linked[A](
      schema: => Value[A],
      validation: Validation[Constraint.Collection, List[A]]
  ): Self[List[A]]

  def constraints[A](self: Self[A]): Chain[Constraint.Collection]

object CollectionOperation:
  inline def apply[Self[_], Value[_]](using
      operation: CollectionOperation[Self, Value]
  ): CollectionOperation[Self, Value] = operation
