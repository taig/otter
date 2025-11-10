package io.taig.otter.component

import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.instance.SchemaInstances.given
import io.taig.otter.instance.SchemaInstances
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.instance.AllInstances

object SchemaComponent
    extends CoerceComponent[Schema.Coerce.Of, Schema],
      CollectionComponent[Schema.Collection.Of, Schema],
      DictionaryComponent[Schema.Dictionary.Of, Schema]

object dsl extends AllInstances, AllSyntax:
  val schema = SchemaComponent

object Playground:
  import dsl.*

  val name: Schema[String] = ???
  val names: Schema.Collection[List[String]] = schema.collection.list(name)

  val a: Schema.Collection.Of[Schema, List[String]] = schema.collection.list(name)
