package io.taig.otter.component

import io.taig.otter.operation.TupleSchemaInvariant

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchemaInvariant[Self, Value]):
  final def TNil: Self[Unit] = self.empty
