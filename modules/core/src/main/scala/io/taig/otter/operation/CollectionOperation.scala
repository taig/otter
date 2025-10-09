package io.taig.otter.operation

import io.taig.validation.Constraint
import io.taig.validation.Validation
import io.taig.otter.OperationInvariant
import cats.data.Chain

trait CollectionOperation[Shape[_], Self[_[a] <: Shape[a], _]]:
  def indexed[Value[a] <: Shape[a], A](
      schema: => Value[A],
      validation: Validation[Constraint.Collection, A]
  ): Self[Value, Vector[A]]

  def linked[Value[a] <: Shape[a], A](
      schema: => Value[A],
      validation: Validation[Constraint.Collection, A]
  ): Self[Value, List[A]]

  def constraints[A](self: Self[Shape, A]): Chain[Constraint.Collection]

object CollectionOperation:
  inline def apply[Shape[_], Self[_[a] <: Shape[a], _]](using
      operation: CollectionOperation[Shape, Self]
  ): CollectionOperation[Shape, Self] = operation

  given OperationInvariant[CollectionOperation] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: CollectionOperation[Shape, Self])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): CollectionOperation[Shape, T] = new CollectionOperation[Shape, T]:
        override def indexed[Value[a] <: Shape[a], A](
            schema: => Value[A],
            validation: Validation[Constraint.Collection, A]
        ): T[Value, Vector[A]] = fK(operation.indexed(schema, validation))

        override def linked[Value[a] <: Shape[a], A](
            schema: => Value[A],
            validation: Validation[Constraint.Collection, A]
        ): T[Value, List[A]] = fK(operation.linked(schema, validation))

        override def constraints[A](self: T[Shape, A]): Chain[Constraint.Collection] =
          operation.constraints(gK(self))
