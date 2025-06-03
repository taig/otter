package io.taig.otter.component

import io.taig.otter.operation.NullableSchemaInvariant

trait NullableComponent[Self[_], -Value[_]](using self: NullableSchemaInvariant[Self, Value]):
  final def void: Self[Unit] = self.void
