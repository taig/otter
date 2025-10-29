package io.taig.otter.syntax

import io.taig.otter.Json

trait JsonSyntax
    extends CoerceSyntax[Json.Coerce, Json.Primitive],
      CollectionSyntax[Json.Collection, Json],
      ConstantSyntax[Json.Constant, Json.Primitive],
      DictionarySyntax[Json.Dictionary, Json],
      EnumerationSyntax[Json.Enumeration, Json.Primitive],
      FieldSyntax[Json.Field, Json.Record, Json],
      NullableSyntax[Json.Nullable, Json],
      RecordSyntax[Json.Record, Json.Field],
      TupleSyntax[Json.Tuple, Json],
      UnionSyntax[Json.Union, Json]

object JsonSyntax extends JsonSyntax
