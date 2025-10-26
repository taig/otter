package io.taig.otter.syntax

import io.taig.otter.Json

trait JsonSyntax extends FieldSyntax[Json.Field, Json.Record, Json], RecordSyntax[Json.Record, Json.Field]

object JsonSyntax extends JsonSyntax
