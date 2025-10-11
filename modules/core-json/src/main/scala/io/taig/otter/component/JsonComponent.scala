package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent extends CollectionComponent[Json.Collection, Json], PrimitiveComponent[Json.Primitive]

object JsonComponent extends JsonComponent
