package io.taig.otter.component

import io.taig.otter as Self
import io.taig.otter.shape.SchemaShape
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.Tupleable

object SchemaComponent
    extends FieldComponent[Schema.Field.Of, Schema](using ???),
      RecordComponent[Schema.Record.Of, Schema](using ???)

object dsl extends AllSyntax, SchemaShape:
  val schema = SchemaComponent

object Playground:
  import dsl.*

  val name: Schema.Primitive.Text[String] = ???
  val names: Schema.Collection[List[String]] = ???

  val age: Schema.Primitive[Int] = ???

  val myField: Schema.Field.Of[Schema.Primitive.Text, String] = schema.field("name", name)

  val myRecord: Schema.Record.Of[Schema.Primitive, String] = myField.toRecord

  // val rec1: Schema.Record.Of[Schema.Primitive, (String, Int)] =
  //   schema.field("name", name) :* schema.field("age", age) // :* schema.field("name", name)

  // val rec2: Schema.Record.Of[Schema.Primitive, (String, Int)] =
  //   schema.field("name", name) *: schema.field("age", age)

  val _: Schema.Tuple.Of[Schema.Primitive.Text, String] = name.toTuple
  val _: Schema.Tuple.Of[Schema.Primitive, String] = name.toTuple
  val _: Schema.Tuple.Of[Schema.Collection, List[String]] = names.toTuple

  val _: Schema.Tuple.Of[Schema.Tuple.Of[Schema.Primitive.Text, *], String] = name.toTuple.toTuple
  val _: Schema.Tuple.Of[Schema, (List[String], String)] = names :* name
  val _: Schema.Tuple.Of[Schema, (List[String], String)] = names :* name
  val _: Schema.Tuple.Of[[a] =>> Schema.Collection[a] | Schema.Primitive[a], (List[String], String)] = names :* name
  val _: Schema.Tuple.Of[[a] =>> Schema.Collection[a] | Schema.Primitive[a], (String, List[String])] = name :* names
  val a: Schema.Tuple.Of[Schema.Primitive, (Int, String)] = age :* name
  val _: Schema.Tuple.Of[Schema.Primitive, (String, Int)] = name :* age
  val _: Schema.Tuple.Of[Schema.Primitive.Text, (String, String)] = name :* name
