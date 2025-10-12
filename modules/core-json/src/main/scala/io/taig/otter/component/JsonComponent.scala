package io.taig.otter.component

import io.taig.otter.shape.JsonShape.Json

trait JsonComponent
    extends BooleanComponent[Json.Primitive.Boolean],
      CoerceComponent[Json.Primitive, Json.Coerce.Of],
      CollectionComponent[Json, Json.Collection.Of],
      ConstantComponent[Json.Primitive, Json.Constant.Of],
      DictionaryComponent[Json, Json.Dictionary.Of],
      EnumerationComponent[Json.Primitive, Json.Enumeration.Of],
      NumberComponent[Json.Primitive.Number],
      PrimitiveComponent[Json.Primitive],
      RecordComponent[Json, Json.Record.Of],
      StringComponent[Json.Primitive.String],
      TupleComponent[Json, Json.Tuple.Of],
      UnionComponent[Json, Json.Union.Of]

object JsonComponent extends JsonComponent
