package io.taig.otter.component

import io.taig.otter.operation.RecordOperation

trait RecordComponent[+Self[_], -Value[_]](using RecordOperation[Self, Value])
