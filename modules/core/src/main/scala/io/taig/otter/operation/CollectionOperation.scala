package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.InvariantK
import io.taig.validation.Constraint
import io.taig.validation.Constraint.Collection
import io.taig.validation.Validation

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

  given [Value[_]]: InvariantK[[f[_]] =>> CollectionOperation[f, Value]] with
    extension [G[_]](operation: CollectionOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CollectionOperation[H, Value] =
        new CollectionOperation[H, Value]:
          override def indexed[A](schema: => Value[A], validation: Validation[Collection, Vector[A]]): H[Vector[A]] =
            fK(operation.indexed(schema, validation))

          override def linked[A](
              schema: => Value[A],
              validation: Validation[Collection, List[A]]
          ): H[List[A]] = fK(operation.linked(schema, validation))

          override def constraints[A](self: H[A]): Chain[Collection] = operation.constraints(gK(self))
