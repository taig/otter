package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.DictionaryComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.FieldComponent
import io.taig.otter.component.KeyComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.RecordComponent
import io.taig.otter.component.UnionComponent
import io.taig.otter.http.FormData

trait FormDataComponent
    extends ConstantComponent[FormData.Schema.Constant, FormData.Schema],
      DictionaryComponent[FormData.Dictionary, Key, FormData.Schema],
      EnumerationComponent[FormData.Schema.Enumeration, FormData.Schema],
      FieldComponent[FormData.Field, Key, FormData.Schema, FormData.Record],
      FieldComponent.Primitive.String[FormData.Field, Key, FormData.Schema, FormData.Record],
      NullableComponent[FormData.Schema.Nullable, FormData.Schema],
      PrimitiveComponent[FormData.Schema.Primitive, FormData.Schema.Primitive.String],
      PrimitiveComponent.Boolean[FormData.Schema.Primitive.Boolean],
      PrimitiveComponent.Number[FormData.Schema.Primitive.Number],
      PrimitiveComponent.String[FormData.Schema.Primitive.String, FormData.Schema.Primitive],
      RecordComponent[FormData.Record, FormData.Field],
      UnionComponent[FormData.Schema.Union, FormData.Schema]:
  override def key: KeyComponent = KeyComponent

object FormDataComponent extends FormDataComponent
