package io.taig.otter.syntax

import io.taig.otter.operation.RecordOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.Reference

trait FieldSyntax[Self[_], +Record[_], +Value[_]](using
    field: FieldOperation[Self, Value],
    record: RecordOperation[Record, Self]
):
  extension [A](self: Self[A])
    def name: String = field.name(self)
    def schema: Reference[Value, ?] = field.schema(self)
    def toRecord: Record[A] = record.lift(self)
