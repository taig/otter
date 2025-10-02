package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent extends FieldComponent[Json.Field, Json]

object JsonComponent extends JsonComponent
