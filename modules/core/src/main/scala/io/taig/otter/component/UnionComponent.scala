package io.taig.otter.component

import io.taig.otter.operation.UnionOperation

trait UnionComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using UnionOperation[Shape, Self])
