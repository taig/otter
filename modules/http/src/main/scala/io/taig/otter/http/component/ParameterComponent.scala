package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Parameter

trait ParameterComponent
    extends CollectionComponent[Parameter.Schema.Array.Collection, Parameter.Schema.Value],
      ConstantComponent[Parameter.Schema.Value.Constant, Parameter.Schema.Value.Primitive],
      DictionaryComponent[Parameter.Schema.Object.Dictionary, Key, Parameter.Schema.Value],
      EnumerationComponent[Parameter.Schema.Value.Enumeration, Parameter.Schema.Value.Primitive],
      PrimitiveComponent.String[Parameter.Schema.Value.Primitive],
      FieldComponent.Primitive.String[
        Parameter.Schema.Field,
        Key,
        Parameter.Schema.Object.Value,
        Parameter.Schema.Object.Record
      ],
      TupleComponent[Parameter.Schema.Array.Tuple, Parameter.Schema.Value]:
  override def key: KeyComponent = KeyComponent

object ParameterComponent extends ParameterComponent
