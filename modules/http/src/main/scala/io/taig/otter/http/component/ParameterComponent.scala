package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Parameter

trait ParameterComponent
    extends CollectionComponent[Parameter.Value.Array.Collection, Parameter.Value.Atom],
      ConstantComponent[Parameter.Value.Atom.Constant, Parameter.Value.Atom.Primitive],
      DictionaryComponent[Parameter.Value.Object.Dictionary, Key, Parameter.Value.Atom],
      EnumerationComponent[Parameter.Value.Atom.Enumeration, Parameter.Value.Atom.Primitive],
      PrimitiveComponent.String[Parameter.Value.Atom.Primitive],
      FieldComponent.Primitive.String[
        Parameter.Value.Field,
        Key,
        Parameter.Value.Object.Atom,
        Parameter.Value.Object.Record
      ],
      TupleComponent[Parameter.Value.Array.Tuple, Parameter.Value.Atom]:
  override def key: KeyComponent = KeyComponent

object ParameterComponent extends ParameterComponent
