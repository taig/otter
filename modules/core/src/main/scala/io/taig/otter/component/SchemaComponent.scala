package io.taig.otter.component

import io.taig.otter.Annotation
import io.taig.otter.Coerce
import io.taig.otter.InvariantK2
import io.taig.otter.Wrapper
import io.taig.otter.base as Base
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.Reference
import io.taig.otter.shape.SchemaShape

object dsl extends AllSyntax:
  val schema = SchemaComponent

import dsl.*

object SchemaComponent
    extends CoerceComponent[Schema.Coerce.Of, Schema],
      CollectionComponent[Schema.Collection.Of, Schema],
      ConstantComponent[Schema.Constant.Of, Schema],
      DictionaryComponent[Schema.Dictionary.Of, Schema],
      NullableComponent[Schema.Nullable.Of, Schema]

object Playground:

  val name: Schema[String] = ???
  val names: Schema.Collection[List[String]] = schema.collection.list(name)

  val x = name.coerce
  // name.coerceLol

  val coercedName = schema.coerce(name)
  // a.nullable
