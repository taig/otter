package io.taig.otter

import munit.FunSuite
import io.taig.otter.syntax.AllSyntax.*
import io.taig.otter.component.SchemaComponent.*
import io.taig.otter.shape.SchemaShape.*

final class SchemaTest extends FunSuite:
  test("default"):
    val value: Schema.Primitive.String[String] = string
    val myField: Schema.Field.Of[Schema.Primitive, String] = field("foo", value)
    // given RecordOperation[Schema.Record, Schema.Field] = io.taig.otter.Schema.Record.operation[Schema]
    myField.toRecord
    // val myRecord = myField.toRecord
    // val myRedord: Schema.Record.Of[Schema.Primitive, String] = myField.toRecord
