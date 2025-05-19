package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Http

trait HttpHeaderComponent
    extends CollectionComponent[Http.Header.Array.Collection, Http.Header.Value],
      ConstantComponent[Http.Header.Value.Constant, Http.Header.Value.Primitive],
      DictionaryComponent[Http.Header.Object.Dictionary, Key, Http.Header.Value],
      EnumerationComponent[Http.Header.Value.Enumeration, Http.Header.Value.Primitive],
      PrimitiveComponent.String[Http.Header.Value.Primitive],
      FieldComponent.Primitive.String[Http.Header.Field, Key, Http.Header.Object.Value],
      TupleComponent[Http.Header.Array.Tuple, Http.Header.Value]:
  override def key: KeyComponent = KeyComponent

object HttpHeaderComponent extends HttpHeaderComponent
