package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[Self[_[a] <: Bound[a], _], Bound[_]](using F: TupleOperation[Self, Bound]):
  final def TNil: Self[Nothing, Unit] = F.empty
