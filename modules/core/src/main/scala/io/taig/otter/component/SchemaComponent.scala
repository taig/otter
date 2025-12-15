package io.taig.otter.component

import io.taig.otter as Self
import io.taig.otter.shape.SchemaShape
import io.taig.otter.shape.SchemaShape.*
import io.taig.otter.syntax.AllSyntax

object SchemaComponent
    extends BranchComponent[Schema.Branch.Of, Schema],
      FieldComponent[Schema.Field.Of, Schema],
      RecordComponent[Schema.Record.Of, Schema.Field.Of, Schema],
      UnionComponent[Schema.Union.Of, Schema.Branch.Of, Schema]

object dsl extends AllSyntax, SchemaShape:
  val schema = SchemaComponent

object Playground:
  import dsl.*

  val name: Schema.Primitive.Text[String] = ???
  val names: Schema.Collection[List[String]] = ???

  val age: Schema.Primitive[Int] = ???

  val myField: Schema.Field.Of[Schema.Primitive.Text, String] = schema.field("name", name)
  val myFieldRead: Schema.Field.Read.Of[Schema.Primitive.Text.Read, String] = schema.field("name", name)
  val myBranch: Schema.Branch.Of[Schema.Primitive.Text, String] = schema.branch("name", name)

  val _ = myField.toRecord
  val _: Schema.Record.Of[Schema.Primitive, String] = myField.toRecord
  val _: Schema.Record.Read.Of[Schema.Primitive, String] = myField.toRecord
  val _: Schema.Record.Read.Of[Schema.Read, String] = myFieldRead.toRecord

  val _: Schema.Record[(String, String)] = myField :* myField
  val _: Schema.Record.Read[(String, String)] = (myField: Schema.Field.Read[String]) :* myFieldRead

//   // val rec1: Schema.Record.Of[Schema.Primitive, (String, Int, String)] =
//   //   schema.field("name", name) :* schema.field("age", age) :* schema.field("name", name)

//   // val rec2: Schema.Record.Of[Schema, ((String, Int, String, (String, Int, String)))] =
//   //   schema.field("a", rec1) :* schema.field("b", rec1)

//   // val rec3: Schema.Record.Of[Schema, ((String, Int, String), String, Int)] =
//   //   schema.field("rec", rec1) *: schema.field("name", name) *: schema.field("age", age)

//   // val rec4: Schema.Record.Of[Schema, (String, Int, String, String, Int, String)] =
//   //   schema.field("a", rec1) * schema.field("b", rec1)

//   val _: Schema.Tuple.Of[Schema.Primitive.Text, String] = name.toTuple
//   val _: Schema.Tuple.Of[Schema.Primitive, String] = name.toTuple
//   val _: Schema.Tuple.Of[Schema.Collection, List[String]] = names.toTuple

//   val _: Schema.Tuple.Of[Schema.Tuple.Of[Schema.Primitive.Text, *], String] = name.toTuple.toTuple
//   val _: Schema.Tuple.Of[Schema, (List[String], String)] = names :* name
//   val _: Schema.Tuple.Of[Schema, (List[String], String)] = names :* name
//   val _: Schema.Tuple.Of[[a] =>> Schema.Collection[a] | Schema.Primitive[a], (List[String], String)] = names :* name
//   val _: Schema.Tuple.Of[[a] =>> Schema.Collection[a] | Schema.Primitive[a], (String, List[String])] = name :* names
//   val _: Schema.Tuple.Of[Schema.Primitive, (Int, String)] = age :* name
//   val _: Schema.Tuple.Of[Schema.Primitive, (String, Int)] = name :* age
//   val _: Schema.Tuple.Of[Schema.Primitive.Text, (String, String)] = name :* name
