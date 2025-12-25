package io.taig.otter.component

import io.taig.otter.Schema

trait SchemaComponent
    extends TupleComponent[
      Schema.Tuple.Of,
      Schema.Tuple.Read.Of,
      Schema.Tuple.Write.Of,
      Schema.Of,
      Schema.Read.Of,
      Schema.Write.Of,
      Schema,
      Schema.Read,
      Schema.Write
    ]

object SchemaComponent extends SchemaComponent

object Playground:
  val schema: SchemaComponent = SchemaComponent

  val x: Schema.Primitive[String] = ???
  val y: Schema.Of[Schema, String] = ???

  y :* y
  // :*(y)(y)
  // y :* y
  // x :* x
