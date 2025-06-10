package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.Key

// format: off
trait JsonComponent extends
      ComparisonComponent[Json.Record, Json.Field, Key, Json],
      CollectionComponent[Json.Collection, Json],
      ConstantComponent[Json.Constant, Json.Primitive],
      ConstantComponent.Primitive[Json.Constant, Json.Primitive],
      DictionaryComponent[Json.Dictionary, Key, Json],
      EnumerationComponent[Json.Enumeration, Json.Primitive],
      FieldComponent.Primitive.String[Json.Field, Key, Json],
      NullableComponent[Json.Nullable, Json],
      PrimitiveComponent[Json.Primitive],
      TupleComponent[Json.Tuple, Json],
      ErrorComponent[Json.Constant, Json.Primitive, Json.Record, Json.Field, Key, Json],
      ViolationsComponent[Json.Collection, Json.Constant, Json.Dictionary, Json.Primitive, Json.Record, Json.Union, Json.Field, Key, Json]:
  self =>

  override def key: KeyComponent = KeyComponent

object JsonComponent extends JsonComponent
// format: on
