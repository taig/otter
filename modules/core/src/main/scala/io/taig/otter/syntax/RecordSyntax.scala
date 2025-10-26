package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.RecordOperation

trait RecordSyntax[Self[_], Value[_]](using operation: RecordOperation[Self, Value]):
  extension [A](self: Self[A]) def fields: Chain[Reference[Value, ?]] = operation.fields(self)
