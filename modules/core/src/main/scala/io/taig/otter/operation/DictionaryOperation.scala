package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.otter.FunctorK
import io.taig.validation.Validation

trait DictionaryOperation[+Self[_], -Value[_]]:
  self =>

  def dictionary[A](schema: => Value[A], validation: Validation[Constraint.Object, A]): Self[List[(String, A)]]

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): DictionaryOperation[T, Value] = new DictionaryOperation[T, Value]:
    override def dictionary[A](
        schema: => Value[A],
        validation: Validation[Constraint.Object, A]
    ): T[List[(String, A)]] = fK(self.dictionary(schema, validation))

object DictionaryOperation:
  inline def apply[Self[_], Value[_]](using
      operation: DictionaryOperation[Self, Value]
  ): DictionaryOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[s[_]] =>> DictionaryOperation[s, Value]] with
    extension [G[_]](self: DictionaryOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): DictionaryOperation[H, Value] = self.mapK(fK)
