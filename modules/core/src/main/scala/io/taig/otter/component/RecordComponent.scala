package io.taig.otter.component

import io.taig.otter.operation.RecordOperation

trait RecordComponent[-Shape[_], Self[_[a] <: Shape[a], _]](using RecordOperation[Shape, Self])
