package io.taig.otter.component

import io.taig.otter.schema.NullableSchema

trait NullableComponent[+Self[_], -Value[_]](using self: NullableSchema[Self, Value]):
  final def void: Self[Unit] = self.void
