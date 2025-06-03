package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Parameter

trait ParameterComponent
    extends CollectionComponent[Parameter.Schema.Array.Collection, Parameter.Schema.Value],
      ConstantComponent[Parameter.Schema.Value.Constant, Parameter.Schema.Primitive.String],
      DictionaryComponent[Parameter.Schema.Object.Dictionary, Key, Parameter.Schema.Value],
      EnumerationComponent[Parameter.Schema.Value.Enumeration, Parameter.Schema.Primitive.String],
      PrimitiveComponent[Parameter.Schema.Primitive],
      PrimitiveComponent.Boolean[Parameter.Schema.Primitive.Boolean],
      PrimitiveComponent.Number[Parameter.Schema.Primitive.Number],
      PrimitiveComponent.String[Parameter.Schema.Primitive.String],
      FieldComponent.Primitive.String[Parameter.Schema.Field, Key, Parameter.Schema.Object.Value],
      TupleComponent[Parameter.Schema.Array.Tuple, Parameter.Schema.Value]:
  override def key: KeyComponent = KeyComponent

object ParameterComponent extends ParameterComponent
