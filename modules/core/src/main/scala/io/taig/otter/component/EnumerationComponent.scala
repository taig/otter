package io.taig.otter.component

import io.taig.otter.operation.EnumerationOperation
import io.taig.enumeration.ext.Mapping
import cats.kernel.Order
import io.taig.enumeration.ext.EnumerationValues

trait EnumerationComponent[+Self[_], -Value[_]](using operation: EnumerationOperation[Self, Value]):
  def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B] =
    operation.enumeration(schema, mapping)

  def enumeration[A: Order, B](schema: => Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Self[B] = enumeration(schema, Mapping.enumeration(f))

  def enumeration[A]: EnumerationApply[A] = new EnumerationApply[A]

  final class EnumerationApply[A]:
    def apply[B: Order](schema: => Value[B])(f: A => B)(using
        EnumerationValues.Aux[A, A]
    ): Self[A] = enumeration(schema, Mapping.enumeration(f))
