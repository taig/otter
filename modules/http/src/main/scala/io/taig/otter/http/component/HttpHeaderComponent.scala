package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Header

trait HttpHeaderComponent
    extends CollectionComponent[Header.Schema.Array.Collection, Header.Schema.Atom],
      ConstantComponent[Header.Schema.Atom.Constant, Header.Schema.Atom.Primitive],
      DictionaryComponent[Header.Schema.Object.Dictionary, Key, Header.Schema.Atom],
      EnumerationComponent[Header.Schema.Atom.Enumeration, Header.Schema.Atom.Primitive],
      PrimitiveComponent.String[Header.Schema.Atom.Primitive],
      FieldComponent.Primitive.String[Header.Schema.Field, Key, Header.Schema.Object.Atom, Header.Schema.Object.Record],
      TupleComponent[Header.Schema.Array.Tuple, Header.Schema.Atom]:
  override def key: KeyComponent = KeyComponent

object HttpHeaderComponent extends HttpHeaderComponent
