package io.taig.otter.component

import io.taig.otter.schema.RecordSchema

trait RecordComponent[Self[_], Key[_], Value[_]](using record: RecordSchema[Self, Key, Value])
