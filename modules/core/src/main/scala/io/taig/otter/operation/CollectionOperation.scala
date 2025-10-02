package io.taig.otter.operation

import io.taig.otter.FunctorK
import io.taig.validation.Constraint
import io.taig.validation.Validation

trait CollectionOperation[+Self[_], -Value[_]]:
  self =>

  def indexed[A](schema: => Value[A], validation: Validation[Constraint.Collection, A]): Self[Vector[A]]

  def linked[A](schema: => Value[A], validation: Validation[Constraint.Collection, A]): Self[List[A]]

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): CollectionOperation[T, Value] = new CollectionOperation[T, Value]:
    override def indexed[A](schema: => Value[A], validation: Validation[Constraint.Collection, A]): T[Vector[A]] =
      fK(self.indexed(schema, validation))

    override def linked[A](schema: => Value[A], validation: Validation[Constraint.Collection, A]): T[List[A]] =
      fK(self.linked(schema, validation))

object CollectionOperation:
  inline def apply[Self[_], Value[_]](using
      operation: CollectionOperation[Self, Value]
  ): CollectionOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[s[_]] =>> CollectionOperation[s, Value]] with
    extension [G[_]](self: CollectionOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): CollectionOperation[H, Value] = self.mapK(fK)
