package io.taig.otter

import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping

trait EnumerationDsl[Self[_], -Value[_]](using codec: Codec.Enumeration[Self, Value]):
  self =>

  final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
    self.codec.enumeration(codec, mapping)

  final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
    enumeration(codec)(using Mapping.enumeration(f))
