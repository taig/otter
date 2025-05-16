package io.taig.otter.component

import io.taig.otter.Json

trait JsonKeyComponent
    extends ConstantComponent.Primitive.String[Json.Key.Constant, Json.Key.Primitive],
      EnumerationComponent[Json.Key.Enumeration, Json.Key.Primitive],
      PrimitiveComponent.String[Json.Key.Primitive],
      UnionComponent[Json.Key.Union, Json.Key]

object JsonKeyComponent extends JsonKeyComponent
