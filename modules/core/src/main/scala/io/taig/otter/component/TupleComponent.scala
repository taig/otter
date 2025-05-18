package io.taig.otter.component

import io.taig.otter.schema.TupleSchema
import scala.annotation.targetName
import io.taig.otter.Merge

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  final def TNil: Self[Unit] = self.empty
