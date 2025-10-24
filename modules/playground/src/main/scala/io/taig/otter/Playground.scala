package io.taig.otter

import io.taig.otter.component.IronNumberComponent
import io.taig.otter.component.IronStringComponent
import io.taig.otter.component.JsonComponent

object dsl:
  object json extends JsonComponent:
    object iron extends IronNumberComponent[Json.Primitive.Number], IronStringComponent[Json.Primitive.String]
