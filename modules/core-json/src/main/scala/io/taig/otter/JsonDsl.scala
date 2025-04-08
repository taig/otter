package io.taig.otter

import io.taig.otter.Json.Key

trait JsonDsl
    extends CollectionDsl[Json.Collection, Json],
      ConstantDsl[Json.Constant, Json],
      ConstantDsl.Primitive[Json.Constant, Json.Primitive],
      DictionaryDsl[Json.Dictionary, Json.Key, Json],
      EnumerationDsl[Json.Enumeration, Json.Primitive],
      OptionalDsl[Json.Optional, Json],
      PrimitiveDsl[Json.Primitive],
      RecordDsl[Json.Record, Json.Key, Json],
      RecordDsl.Primitive.String[Json.Record, Json.Key.Primitive, Json],
      TupleDsl[Json.Tuple, Json]:
  final val key: JsonKeyDsl = JsonKeyDsl

object JsonDsl extends JsonDsl
