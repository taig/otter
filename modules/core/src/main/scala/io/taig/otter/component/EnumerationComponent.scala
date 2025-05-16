package io.taig.otter.component

import io.taig.enumeration.ext.Mapping
import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.otter.schema.EnumerationSchema
import cats.data.NonEmptyList
import io.taig.otter.Reference

trait EnumerationComponent[Self[_], Value[_]](using self: EnumerationSchema[Self, Value]):
  final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
    self(codec, mapping)

  final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
    enumeration(codec)(using Mapping.enumeration(f))

  extension [A](self: Self[A])
    def schema: Reference[Value, ?] = this.self.schema(self)
    def values: NonEmptyList[A] = this.self.values(self)
