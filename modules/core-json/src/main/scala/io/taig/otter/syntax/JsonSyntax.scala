package io.taig.otter.syntax

import io.taig.otter.Json

trait JsonSyntax
    extends CoerceSyntax[Json.Coerce, Json.Primitive],
      ConstantSyntax[Json.Constant, Json.Primitive],
      EnumerationSyntax[Json.Enumeration, Json.Primitive],
      FieldSyntax[Json.Field, Json.Record, Json],
      NullableSyntax[Json.Nullable, Json],
      RecordSyntax[Json.Record, Json.Field]

object JsonSyntax extends JsonSyntax
