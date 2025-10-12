package io.taig.otter.component

import io.taig.otter.operation.EnumerationOperation
import io.taig.enumeration.ext.Mapping

trait EnumerationComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using operation: EnumerationOperation[Shape, Self]):
  def enumeration[Value[a] <: Shape[a], A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[Value, B] =
    operation.enumeration(schema, mapping)
