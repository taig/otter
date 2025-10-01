package io.taig.otter.component

import io.taig.otter.operation.BooleanOperation

trait BooleanComponent[+Self[_]](using operation: BooleanOperation[Self]):
  val boolean: Self[Boolean] = operation.boolean
