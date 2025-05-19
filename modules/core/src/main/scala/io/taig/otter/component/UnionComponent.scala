package io.taig.otter.component

import io.taig.otter.schema.UnionSchema

trait UnionComponent[Self[_], Value[_]](using self: UnionSchema[Self, Value]):
  protected given UnionSchema[Self, Value] = self
