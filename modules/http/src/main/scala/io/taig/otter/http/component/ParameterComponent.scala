package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Parameter

trait ParameterComponent
    extends CollectionComponent[Parameter.Schema.Array.Collection, Parameter.Schema.Atom],
      ConstantComponent[Parameter.Schema.Atom.Constant, Parameter.Schema.Atom.Primitive],
      DictionaryComponent[Parameter.Schema.Object.Dictionary, Key, Parameter.Schema.Atom],
      EnumerationComponent[Parameter.Schema.Atom.Enumeration, Parameter.Schema.Atom.Primitive],
      PrimitiveComponent.String[Parameter.Schema.Atom.Primitive],
      FieldComponent.Primitive.String[
        Parameter.Schema.Field,
        Key,
        Parameter.Schema.Object.Atom,
        Parameter.Schema.Object.Record
      ],
      TupleComponent[Parameter.Schema.Array.Tuple, Parameter.Schema.Atom]:
  override def key: KeyComponent = KeyComponent

object ParameterComponent extends ParameterComponent
