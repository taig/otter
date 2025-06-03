package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.Key

// format: off
trait JsonComponent extends
      ComparisonComponent[Json.Record, Json.Field, Key, Json]:
      // CollectionComponent[Json.Collection, Json],
      // ConstantComponent[Json.Constant, Json],
      // ConstantComponent.Primitive[Json.Constant, Json.Primitive, Json.Primitive],
      // DictionaryComponent[Json.Dictionary, Key, Json],
      // EnumerationComponent[Json.Enumeration, Json.Primitive],
      // FieldComponent[Json.Field, Key, Json, Json.Record],
      // FieldComponent.Primitive.String[Json.Field, Key, Json, Json.Record],
      // NullableComponent[Json.Nullable, Json],
      // PrimitiveComponent[Json.Primitive, Json.Primitive],
      // PrimitiveComponent.Boolean[Json.Primitive],
      // PrimitiveComponent.Number[Json.Primitive],
      // PrimitiveComponent.String[Json.Primitive, Json.Primitive],
      // RecordComponent[Json.Record, Json.Field],
      // TupleComponent[Json.Tuple, Json],
      // UnionComponent[Json.Union, Json],
      // ErrorComponent[Json.Constant, Json.Record, Json.Primitive, Json.Field, Key, Json],
      // ViolationsComponent[Json.Collection, Json.Constant, Json.Dictionary, Json.Nullable, Json.Primitive, Json.Record, Json.Union, Json.Field, Key, Json]:
  override def key: KeyComponent = KeyComponent

// object JsonComponent extends JsonComponent
// format: on
