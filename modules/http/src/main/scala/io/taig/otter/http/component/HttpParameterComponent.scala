package io.taig.otter.http.component

import io.taig.otter.Key
import io.taig.otter.component.*
import io.taig.otter.http.Http

trait HttpParameterComponent
    extends CollectionComponent[Http.Parameter.Array.Collection, Http.Parameter.Value],
      ConstantComponent[Http.Parameter.Value.Constant, Http.Parameter.Value.Primitive],
      DictionaryComponent[Http.Parameter.Object.Dictionary, Key, Http.Parameter.Value],
      EnumerationComponent[Http.Parameter.Value.Enumeration, Http.Parameter.Value.Primitive],
      PrimitiveComponent.String[Http.Parameter.Value.Primitive],
      FieldComponent.Primitive.String[Http.Parameter.Field, Key, Http.Parameter.Object.Value],
      TupleComponent[Http.Parameter.Array.Tuple, Http.Parameter.Value]:
  override def key: KeyComponent = KeyComponent

object HttpParameterComponent extends HttpParameterComponent
