// package io.taig.otter

// // format: off
// trait JsonDsl extends
//       ComparisonDsl[Json.Record, Json.Field, Json.Key, Json],
//       CollectionDsl[Json.Collection, Json],
//       ConstantDsl[Json.Constant, Json],
//       ConstantDsl.Primitive[Json.Constant, Json.Primitive],
//       DictionaryDsl[Json.Dictionary, Json.Key, Json],
//       EnumerationDsl[Json.Enumeration, Json.Primitive],
//       NullableDsl[Json.Nullable, Json],
//       PrimitiveDsl[Json.Primitive],
//       FieldDsl[Json.Field, Json.Key, Json , Json.Record],
//       FieldDsl.Primitive.String[Json.Field, Json.Key, Json, Json.Record],
//       TupleDsl[Json.Tuple, Json],
//       // UnionDsl[Json.Union, Json],
//       ErrorDsl[Json.Constant, Json.Record, Json.Field, Json.Key, Json]:
//       // ViolationsDsl[Json.Collection, Json.Dictionary, Json.Nullable, Json.Primitive, Json.Record, Json.Union, Json.Field, Json.Key, Json]:
//   override def key: JsonKeyDsl = JsonKeyDsl

// object JsonDsl extends JsonDsl
// // format: on
