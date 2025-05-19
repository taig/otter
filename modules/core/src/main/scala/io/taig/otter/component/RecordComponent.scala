package io.taig.otter.component
import io.taig.otter.schema.RecordSchema

trait RecordComponent[Self[_], -Field[_]](using self: RecordSchema[Self, Field])
