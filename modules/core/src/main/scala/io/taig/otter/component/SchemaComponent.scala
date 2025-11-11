package io.taig.otter.component

import io.taig.otter.shape.SchemaShape
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax

object SchemaComponent
    extends CoerceComponent[Schema.Coerce.Of, Schema],
      CollectionComponent[Schema.Collection.Of, Schema],
      ConstantComponent[Schema.Constant.Of, Schema],
      DictionaryComponent[Schema.Dictionary.Of, Schema],
      EnumerationComponent[Schema.Enumeration.Of, Schema],
      NullishComponent[Schema.Nullish.Of, Schema],
      RecordComponent[Schema.Record.Of, Schema],
      TupleComponent[Schema.Tuple.Of, Schema],
      UnionComponent[Schema.Union.Of, Schema]

object dsl extends AllSyntax, SchemaShape:
  val schema = SchemaComponent

object Playground:
  import dsl.*

  val name: Schema.Primitive.Text[String] = ???
  val names: Schema.Collection[List[String]] = schema.collection.list(name)

  val coercedName = schema.coerce(name)

  val a = schema.field("foo", name) :* schema.field("foo", name) :* schema.field("foobar", name)
  // val b = name :* a
