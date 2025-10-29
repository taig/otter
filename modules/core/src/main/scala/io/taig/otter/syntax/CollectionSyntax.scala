package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation

trait CollectionSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: CollectionOperation[Self, Value])
    def constraints: Chain[Constraint.Collection] = operation.constraints(self)

    def schema: Reference[Value, ?] = operation.schema(self)

object CollectionSyntax extends CollectionSyntax
