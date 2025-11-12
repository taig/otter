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

  val x: Schema.Dictionary[Unit] = ???

  val myField: Schema.Field.Of[Schema.Primitive.Text, String] = schema.field("name", name)

  val myRecord: Schema.Record.Of[Schema.Primitive, String] = myField.toRecord

  val rec = schema.field("name", name) :* schema.field("age", x) // :* schema.field("name", name)

  // TODO dont resolve nothing as self type for primitives but the primitives themselves!

  val a: Schema.Tuple.Of[Schema, String] = name :* x
  // val t: Schema.Tuple.Of[Schema.Collection, String] = name :* x
  // val u: Self.Schema.Tuple[Schema.Collection, String] = name :* x
