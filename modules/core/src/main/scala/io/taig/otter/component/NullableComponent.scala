package io.taig.otter.component

import io.taig.otter.operation.NullableOperation

trait NullableComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using NullableOperation[Shape, Self])
