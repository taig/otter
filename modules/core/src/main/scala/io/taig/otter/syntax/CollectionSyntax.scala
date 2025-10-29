package io.taig.otter.syntax

import io.taig.otter.operation.CollectionOperation
import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import scala.annotation.targetName

trait CollectionSyntax[Self[_], Value[_]](using operation: CollectionOperation[Self, Value]):
  extension [A](self: Self[A])
    @targetName("collectionConstraints")
    def constraints: Chain[Constraint.Collection] = operation.constraints(self)

    def schema: Reference[Value, ?] = operation.schema(self)
