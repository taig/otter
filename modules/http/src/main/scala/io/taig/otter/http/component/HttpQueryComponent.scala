package io.taig.otter.http.component

import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.NullableComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.TupleComponent
import io.taig.otter.http.Query

trait HttpQueryComponent
    extends CollectionComponent[Query.Value.Array.Collection, Query.Value.Atom],
      ConstantComponent[Query.Value.Atom.Constant, Query.Value.Atom.Primitive],
      EnumerationComponent[Query.Value.Atom.Enumeration, Query.Value.Atom.Primitive],
      NullableComponent[Query.Value.Nullable, Query.Value],
      PrimitiveComponent.String[Query.Value.Atom.Primitive],
      TupleComponent[Query.Value.Array.Tuple, Query.Value.Atom]

object HttpQueryComponent extends HttpQueryComponent
