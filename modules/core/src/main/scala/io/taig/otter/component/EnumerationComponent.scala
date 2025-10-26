package io.taig.otter.component

import cats.kernel.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter.operation.EnumerationOperation

trait EnumerationComponent[+Self[_], -Value[_]](using operation: EnumerationOperation[Self, Value]):
  def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B] =
    operation.enumeration(schema, mapping)

  def enumeration[A: Order, B](schema: => Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Self[B] = enumeration(schema, Mapping.enumeration(f))
