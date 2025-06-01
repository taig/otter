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
      ConstantComponent[Query.Schema.Value.Constant, Query.Schema.Primitive.String],
      EnumerationComponent[Query.Schema.Value.Enumeration, Query.Schema.Primitive.String],
      NullableComponent[Query.Schema.Nullable, Query.Schema],
      PrimitiveComponent[Query.Schema.Primitive, Query.Schema.Primitive.String],
      PrimitiveComponent.Boolean[Query.Schema.Primitive.Boolean],
      PrimitiveComponent.Number[Query.Schema.Primitive.Number],
      PrimitiveComponent.String[Query.Schema.Primitive.String, Query.Schema.Primitive],
      TupleComponent[Query.Schema.Array.Tuple, Query.Schema.Value]

object QueryComponent extends QueryComponent
