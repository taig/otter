package io.taig.otter

trait JsonKeyDsl
    extends ConstantDsl.Primitive.String[Json.Key.Constant, Json.Key.Primitive],
      EnumerationDsl[Json.Key.Enumeration, Json.Key.Primitive],
      PrimitiveDsl.String[Json.Key.Primitive]
// UnionDsl.Untagged[Json.Key.Union, Json.Key]

object JsonKeyDsl extends JsonKeyDsl
