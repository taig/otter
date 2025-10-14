package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent
    extends BooleanComponent[Json.Primitive.Boolean],
      CoerceComponent[Json.Coerce, Json.Primitive],
      CollectionComponent[Json.Collection, Json],
      ConstantComponent[Json.Constant, Json.Primitive],
      DictionaryComponent[Json.Dictionary, Json],
      EnumerationComponent[Json.Enumeration, Json.Primitive],
      NumberComponent[Json.Primitive.Number],
      PrimitiveComponent[Json.Primitive],
      RecordComponent[Json.Record, Json.Field],
      StringComponent[Json.Primitive.String],
      TupleComponent[Json.Tuple, Json],
      UnionComponent[Json.Union, Json]

object JsonComponent extends JsonComponent
