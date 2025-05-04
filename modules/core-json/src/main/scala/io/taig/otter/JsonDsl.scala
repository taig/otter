package io.taig.otter


// format: off
trait JsonDsl extends
      ComparisonDsl[Json.Record, Json.Key, Json],
      CollectionDsl[Json.Collection, Json],
      ConstantDsl[Json.Constant, Json],
      ConstantDsl.Primitive[Json.Constant, Json.Primitive],
      DictionaryDsl[Json.Dictionary, Json.Key, Json],
      EnumerationDsl[Json.Enumeration, Json.Primitive],
      NullableDsl[Json.Nullable, Json],
      PrimitiveDsl[Json.Primitive],
      RecordDsl[Json.Record, Json.Key, Json],
      RecordDsl.Primitive.String[Json.Record, Json.Key, Json],
      TupleDsl[Json.Tuple, Json],
      UnionDsl[Json.Union, Json],
      ErrorDsl[Json.Constant, Json.Record, Json.Key, Json],
      ViolationsDsl[ Json.Collection, Json.Dictionary, Json.Nullable, Json.Primitive, Json.Record, Json.Union, Json.Key, Json]:
  override def key: JsonKeyDsl = JsonKeyDsl

object JsonDsl extends JsonDsl
// format: on
