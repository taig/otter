package io.taig.otter.component

import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax.*

object SchemaComponent extends CollectionComponent[Schema.Collection.Of, Schema]

object Playground:
  import SchemaComponent.*

  val name: Schema[String] = ???

  val names: Schema.Collection[List[String]] = collection.list(name)

  val xxlNames: Schema.Collection.Of[Schema, List[String]] = names ++ names
