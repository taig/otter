package io.taig.otter.syntax

import io.taig.otter.operation.TupleOperation
import cats.data.Chain
import io.taig.otter.Reference
import scala.annotation.targetName

trait TupleSyntax[Self[_], Value[_]](using operation: TupleOperation[Self, Value]):
  extension [A](self: Self[A])
    @targetName("tupleSchemas")
    def schemas: Chain[Reference[Value, ?]] = operation.schemas(self)
