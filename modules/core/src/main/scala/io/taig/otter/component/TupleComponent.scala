package io.taig.otter.component
import io.taig.otter.schema.TupleSchema

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  protected given TupleSchema[Self, Value] = self

  final def TNil: Self[Unit] = self.empty
