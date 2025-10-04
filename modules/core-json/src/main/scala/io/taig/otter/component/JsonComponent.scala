package io.taig.otter.component

import io.taig.otter.shape.JsonShape.Json

trait JsonComponent extends FieldComponent[Json, Json.Field.Of]

object JsonComponent extends JsonComponent
