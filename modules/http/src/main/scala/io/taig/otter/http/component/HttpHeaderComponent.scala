package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Header

trait HttpHeaderComponent
    extends CollectionComponent[Header.Schema.Array.Collection, Header.Schema.Value],
      ConstantComponent[Header.Schema.Value.Constant, Header.Schema.Value.Primitive],
      DictionaryComponent[Header.Schema.Object.Dictionary, Key, Header.Schema.Value],
      EnumerationComponent[Header.Schema.Value.Enumeration, Header.Schema.Value.Primitive],
      PrimitiveComponent.String[Header.Schema.Value.Primitive],
      FieldComponent.Primitive.String[
        Header.Schema.Field,
        Key,
        Header.Schema.Object.Value,
        Header.Schema.Object.Record
      ],
      TupleComponent[Header.Schema.Array.Tuple, Header.Schema.Value]:
  override def key: KeyComponent = KeyComponent

object HttpHeaderComponent extends HttpHeaderComponent
