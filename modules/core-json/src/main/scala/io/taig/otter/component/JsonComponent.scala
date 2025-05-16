package io.taig.otter.component

import io.taig.otter.Json

// format: off
trait JsonComponent extends
      ComparisonComponent[Json.Nullable, Json.Record, Json.Field, Json.Key, Json],
      CollectionComponent[Json.Collection, Json],
      ConstantComponent[Json.Constant, Json],
      ConstantComponent.Primitive[Json.Constant, Json.Primitive],
      DictionaryComponent[Json.Dictionary, Json.Key, Json],
      EnumerationComponent[Json.Enumeration, Json.Primitive],
      NullableComponent[Json.Nullable, Json],
      PrimitiveComponent[Json.Primitive],
      FieldComponent[Json.Field, Json.Key, Json , Json.Record],
      FieldComponent.Primitive.String[Json.Field, Json.Key, Json, Json.Record],
      TupleComponent[Json.Tuple, Json],
      // UnionComponent[Json.Union, Json],
      ErrorComponent[Json.Constant, Json.Record, Json.Field, Json.Key, Json]:
      // ViolationsComponent[Json.Collection, Json.Dictionary, Json.Nullable, Json.Primitive, Json.Record, Json.Union, Json.Field, Json.Key, Json]:
  override def key: JsonKeyComponent = JsonKeyComponent

object JsonComponent extends JsonComponent
// format: on
