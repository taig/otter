package io.taig.otter.component

import io.taig.otter.schema.RecordSchema

trait RecordComponent[Self[_], Field[_]](using self: RecordSchema[Self, Field]):
  extension [A](field: Field[A]) def toRecord: Self[A] = self.record(field)
