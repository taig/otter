package io.taig.otter.http.component

import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.TupleComponent
import io.taig.otter.http.Query

trait HttpQueryComponent
    extends CollectionComponent[Query.Schema.Array.Collection, Query.Schema.Value],
      ConstantComponent[Query.Schema.Value.Constant, Query.Schema.Value.Primitive],
      EnumerationComponent[Query.Schema.Value.Enumeration, Query.Schema.Value.Primitive],
      NullableComponent[Query.Schema.Nullable, Query.Schema],
      PrimitiveComponent.String[Query.Schema.Value.Primitive],
      TupleComponent[Query.Schema.Array.Tuple, Query.Schema.Value]

object HttpQueryComponent extends HttpQueryComponent
