package io.taig.otter

import io.taig.enumeration.ext.Mapping
import cats.Order
import io.taig.enumeration.ext.EnumerationValues

trait EnumerationDsl[Self[_], -Value[_]]:
  protected def fromEnumeration[A](self: Enumeration[Value, A]): Self[A]

  final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
    fromEnumeration(Enumeration.Root(codec = Reference.later(codec), mapping, metadata = Metadata.Empty))

  final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
    enumeration(codec)(using Mapping.enumeration(f))
