package io.taig.otter.http.component

import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.TupleComponent
import io.taig.otter.http.Query

trait HttpQueryComponent
    extends CollectionComponent[Query.Schema.Array.Collection, Query.Schema.Atom],
      ConstantComponent[Query.Schema.Atom.Constant, Query.Schema.Atom.Primitive],
      EnumerationComponent[Query.Schema.Atom.Enumeration, Query.Schema.Atom.Primitive],
      NullableComponent[Query.Schema.Nullable, Query.Schema],
      PrimitiveComponent.String[Query.Schema.Atom.Primitive],
      TupleComponent[Query.Schema.Array.Tuple, Query.Schema.Atom]

object HttpQueryComponent extends HttpQueryComponent
