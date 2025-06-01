package io.taig.otter.http.component

import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.TupleComponent
import io.taig.otter.http.Query

trait QueryComponent
    extends CollectionComponent[Query.Schema.Array.Collection, Query.Schema.Value],
      ConstantComponent[Query.Schema.Value.Constant, Query.Schema.Value.String],
      EnumerationComponent[Query.Schema.Value.Enumeration, Query.Schema.Value.String],
      NullableComponent[Query.Schema.Nullable, Query.Schema],
      PrimitiveComponent[Query.Schema.Any, Query.Schema.Value.String],
      PrimitiveComponent.Boolean[Query.Schema.Any.Boolean],
      PrimitiveComponent.Number[Query.Schema.Any.Number],
      PrimitiveComponent.String[Query.Schema.Value.String],
      TupleComponent[Query.Schema.Array.Tuple, Query.Schema.Value]

object QueryComponent extends QueryComponent
