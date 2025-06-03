package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.FormData

trait FormDataComponent
    extends ConstantComponent[FormData.Schema.Constant, FormData.Schema],
      DictionaryComponent[FormData.Dictionary, Key, FormData.Schema],
      EnumerationComponent[FormData.Schema.Enumeration, FormData.Schema],
      FieldComponent[FormData.Field, Key, FormData.Schema],
      FieldComponent.Primitive.String[FormData.Field, Key, FormData.Schema],
      NullableComponent[FormData.Schema.Nullable, FormData.Schema],
      PrimitiveComponent[FormData.Schema.Primitive],
      PrimitiveComponent.Boolean[FormData.Schema.Primitive.Boolean],
      PrimitiveComponent.Number[FormData.Schema.Primitive.Number],
      PrimitiveComponent.String[FormData.Schema.Primitive.String]:
  override def key: KeyComponent = KeyComponent

object FormDataComponent extends FormDataComponent
