package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Header

trait HeaderComponent
    extends CollectionComponent[Header.Schema.Array.Collection, Header.Schema.Value],
      ConstantComponent[Header.Schema.Value.Constant, Header.Schema.Value.String],
      DictionaryComponent[Header.Schema.Object.Dictionary, Key, Header.Schema.Value],
      EnumerationComponent[Header.Schema.Value.Enumeration, Header.Schema.Value.String],
      PrimitiveComponent[Header.Schema.Any],
      PrimitiveComponent.Boolean[Header.Schema.Any.Boolean],
      PrimitiveComponent.Number[Header.Schema.Any.Number],
      PrimitiveComponent.String[Header.Schema.Value.String],
      FieldComponent.Primitive.String[
        Header.Schema.Field,
        Key,
        Header.Schema.Object.Value,
        Header.Schema.Object.Record
      ],
      TupleComponent[Header.Schema.Array.Tuple, Header.Schema.Value]:
  override def key: KeyComponent = KeyComponent

object HeaderComponent extends HeaderComponent
