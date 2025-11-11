package io.taig.otter.component

import io.taig.otter.Annotation
import io.taig.otter.Coerce
import io.taig.otter.base as Base
import io.taig.otter.shape.SchemaShape
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.instances.AllInstances

object SchemaComponent
    extends CoerceComponent[Schema.Coerce.Of, Schema],
      CollectionComponent[Schema.Collection.Of, Schema],
      ConstantComponent[Schema.Constant.Of, Schema],
      DictionaryComponent[Schema.Dictionary.Of, Schema],
      EnumerationComponent[Schema.Enumeration.Of, Schema],
      NullableComponent[Schema.Nullable.Of, Schema],
      RecordComponent[Schema.Record.Of, Schema]

object dsl extends AllInstances, AllSyntax, SchemaShape:
  val schema = SchemaComponent

object Playground:
  import dsl.*
  // import AllInstances.given

  val name: Schema[String] = ???
  val names: Schema.Collection[List[String]] = schema.collection.list(name)

  val x = name.coerce
  // name.coerceLol

  val coercedName = schema.coerce(name)

  val a = schema.field("foo", name) :* schema.field("foo", name)
  // a.nullable
