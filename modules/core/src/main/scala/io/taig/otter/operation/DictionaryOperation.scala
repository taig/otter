package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.otter.InvariantK

trait DictionaryOperation[Self[_], -Value[_]]:
  def dictionary[A](
      schema: => Value[A],
      validation: Validation[Constraint.Object, List[(String, ?)]]
  ): Self[List[(String, A)]]

  def constraints[A](self: Self[A]): Chain[Constraint.Object]

object DictionaryOperation:
  inline def apply[Self[_], Value[_]](using
      operation: DictionaryOperation[Self, Value]
  ): DictionaryOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> DictionaryOperation[f, Value]] with
    extension [G[_]](operation: DictionaryOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): DictionaryOperation[H, Value] =
        new DictionaryOperation[H, Value]:
          override def dictionary[A](
              schema: => Value[A],
              validation: Validation[Constraint.Object, List[(String, ?)]]
          ): H[List[(String, A)]] =
            fK(operation.dictionary(schema, validation))

          override def constraints[A](self: H[A]): Chain[Constraint.Object] = operation.constraints(gK(self))
