package io.taig.otter.component

import io.taig.otter.shape.SchemaShape
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax
import io.taig.otter as Self

object SchemaComponent
    extends FieldComponent[Schema.Field.Of, Schema](using ???),
      RecordComponent[Schema.Record.Of, Schema]

object dsl extends AllSyntax, SchemaShape:
  val schema = SchemaComponent

object Playground:
  import dsl.*

  val name: Schema.Primitive.Text[String] = ???
  val names: Schema.Collection[List[String]] = ???

  val x: Schema.Primitive[Unit] = ???

  val myField: Schema.Field.Of[Schema.Primitive.Text, String] = schema.field("name", name)

  val myRecord: Schema.Record.Of[Schema.Primitive, String] = myField.toRecord

  val rec: Schema.Record.Of[Schema.Primitive, String] =
    schema.field("name", name) :* schema.field("age", x) // :* schema.field("name", name)

  val t: Schema.Tuple.Of[Schema.Tuple.Of[Schema, *], String] = name.toTuple.toTuple.toTuple
  // val u: Schema.Tuple.Of[Schema.Record, (String, String)] = rec.:*(rec)
  val u = name :* x
  // val t: Schema.Tuple.Of[Schema.Primitive, String] = name.toTuple2

  // val a: Schema.Tuple.Of[Schema, String] = name :* x
  // val t: Schema.Tuple.Of[Schema.Collection, String] = name :* x
  // val u: Self.Schema.Tuple[Schema.Collection, String] = name :* x
