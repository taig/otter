package io.taig.otter.syntax

import io.taig.otter.operation.RecordOperation

trait FieldSyntax[Self[_], Record[_]](using operation: RecordOperation[Record, Self]):
  extension [A](self: Self[A]) def toRecord: Record[A] = operation.lift(self)
