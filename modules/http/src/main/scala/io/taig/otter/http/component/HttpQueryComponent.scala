package io.taig.otter.http.component

import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.TupleComponent
import io.taig.otter.http.Http

trait HttpQueryComponent
    extends CollectionComponent[Http.Query.Array.Collection, Http.Query.Value],
      ConstantComponent[Http.Query.Value.Constant, Http.Query.Value.Primitive],
      EnumerationComponent[Http.Query.Value.Enumeration, Http.Query.Value.Primitive],
      NullableComponent[Http.Query.Nullable, Http.Query],
      PrimitiveComponent.String[Http.Query.Value.Primitive],
      TupleComponent[Http.Query.Array.Tuple, Http.Query.Value]

object HttpQueryComponent extends HttpQueryComponent
