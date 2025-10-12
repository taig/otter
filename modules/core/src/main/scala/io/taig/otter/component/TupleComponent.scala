package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using TupleOperation[Shape, Self])
