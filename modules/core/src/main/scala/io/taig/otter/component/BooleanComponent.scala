package io.taig.otter.component

import io.taig.otter.operation.BooleanSchemaInvariant

trait BooleanComponent[+Self[_]](using schema: BooleanSchemaInvariant[Self]):
  val boolean: Self[Boolean] = schema.boolean
