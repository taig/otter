package io.taig.otter.component

import io.taig.otter.Schema
import io.taig.enumeration.ext.Mapping
import cats.Order
import io.taig.enumeration.ext.EnumerationValues

trait EnumerationComponent[+Self[_], -Value[_]](using self: Schema.Enumeration[Self, Value]):
  final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
    self.enumeration(codec, mapping)

  final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
    enumeration(codec)(using Mapping.enumeration(f))
