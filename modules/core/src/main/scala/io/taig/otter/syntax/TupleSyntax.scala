package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.TupleOperation

import scala.annotation.targetName

trait TupleSyntax[Self[_], Value[_]](using operation: TupleOperation[Self, Value]):
  extension [A](self: Self[A])
    @targetName("tupleSchemas")
    def schemas: Chain[Reference[Value, ?]] = operation.schemas(self)
