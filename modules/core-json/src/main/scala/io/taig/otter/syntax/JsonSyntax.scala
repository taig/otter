package io.taig.otter.syntax

import io.taig.otter.Json

trait JsonSyntax extends FieldSyntax[Json.Field, Json.Record]

object JsonSyntax extends JsonSyntax
