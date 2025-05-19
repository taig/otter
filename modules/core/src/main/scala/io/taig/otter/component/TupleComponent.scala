package io.taig.otter.component

import io.taig.otter.schema.TupleSchema
import io.taig.otter.Merge
import io.taig.otter.syntax.InvariantSyntax.*

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  final def TNil: Self[Unit] = self.empty

  export self.:*
