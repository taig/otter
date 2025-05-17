package io.taig.otter.http.component

import io.taig.otter.http.FormData
import io.taig.otter.Key
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.RecordComponent
import io.taig.otter.component.UnionComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.DictionaryComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.FieldComponent
import io.taig.otter.component.KeyComponent

trait FormDataComponent
    extends ConstantComponent[FormData.Value.Constant, FormData.Value],
      DictionaryComponent[FormData.Dictionary, Key, FormData.Value],
      EnumerationComponent[FormData.Value.Enumeration, FormData.Value],
      FieldComponent[FormData.Field, Key, FormData.Value, FormData.Record],
      FieldComponent.Primitive.String[FormData.Field, Key, FormData.Value, FormData.Record],
      NullableComponent[FormData.Value.Nullable, FormData.Value],
      PrimitiveComponent.String[FormData.Value.Primitive],
      RecordComponent[FormData.Record, FormData.Field],
      UnionComponent[FormData.Value.Union, FormData.Value]:
  override def key: KeyComponent = KeyComponent

object FormDataComponent extends FormDataComponent
