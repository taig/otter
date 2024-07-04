package io.taig.otter

import io.taig.otter.Plain.*
import io.taig.otter as Base

object StringEncoder:
  def apply[A](schema: Value.Required.Writer[A], a: A) = schema match
    case schema: Primitive.Required.Writer[A]   => PrimitiveStringEncoder(schema, a)
    case schema: Union.Value.Required.Writer[A] => UnionStringEncoder(schema, a)
    case schema: Enumeration.Required.Writer[A] => EnumerationStringEncoder(schema, a)
