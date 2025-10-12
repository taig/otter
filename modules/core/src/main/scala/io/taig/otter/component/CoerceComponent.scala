package io.taig.otter.component

import io.taig.otter.operation.CoerceOperation

trait CoerceComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using CoerceOperation[Shape, Self])
