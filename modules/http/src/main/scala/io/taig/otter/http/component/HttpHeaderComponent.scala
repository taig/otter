package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Header

trait HttpHeaderComponent
    extends CollectionComponent[Header.Value.Array.Collection, Header.Value.Atom],
      ConstantComponent[Header.Value.Atom.Constant, Header.Value.Atom.Primitive],
      DictionaryComponent[Header.Value.Object.Dictionary, Key, Header.Value.Atom],
      EnumerationComponent[Header.Value.Atom.Enumeration, Header.Value.Atom.Primitive],
      PrimitiveComponent.String[Header.Value.Atom.Primitive],
      FieldComponent.Primitive.String[Header.Value.Field, Key, Header.Value.Object.Atom, Header.Value.Object.Record],
      TupleComponent[Header.Value.Array.Tuple, Header.Value.Atom]:
  override def key: KeyComponent = KeyComponent

object HttpHeaderComponent extends HttpHeaderComponent
