package io.taig.otter

import io.taig.otter as Base

trait SchemaResolvers extends Schemas:
  type SchemaResolver[A] = Base.SchemaResolver[Schema.Writer, A]

  given SchemaResolver[Int] = SchemaResolver(int)
  given SchemaResolver[Long] = SchemaResolver(long)
  given SchemaResolver[String] = SchemaResolver(string)
