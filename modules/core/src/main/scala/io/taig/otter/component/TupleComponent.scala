package io.taig.otter.component

import io.taig.otter.schema.TupleSchema

trait TupleComponent[+Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  final def TNil: Self[Unit] = self.empty

  extension [A](value: Value[A]) final def toTuple: Self[A] = self.one(value)
