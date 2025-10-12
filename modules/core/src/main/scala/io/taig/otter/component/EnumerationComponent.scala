package io.taig.otter.component

import io.taig.otter.operation.EnumerationOperation
import io.taig.enumeration.ext.Mapping
import cats.kernel.Order
import io.taig.enumeration.ext.EnumerationValues

trait EnumerationComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using operation: EnumerationOperation[Shape, Self]):
  def enumeration[Value[a] <: Shape[a], A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[Value, B] =
    operation.enumeration(schema, mapping)

  def enumeration[Value[a] <: Shape[a], A: Order, B](schema: => Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Self[Value, B] = enumeration(schema, Mapping.enumeration(f))

  def enumeration[A]: EnumerationApply[A] = new EnumerationApply[A]

  final class EnumerationApply[A]:
    def apply[Value[a] <: Shape[a], B: Order](schema: => Value[B])(f: A => B)(using
        EnumerationValues.Aux[A, A]
    ): Self[Value, A] = enumeration(schema, Mapping.enumeration(f))
