package io.taig.otter

import io.taig.otter.Plain.*
import io.taig.otter as Base

object ValueRequiredStringEncoder:
  def apply[A](schema: Value.Required.Writer[A], a: A): String = schema match
    case schema: Enumeration.Required.Writer[A] => EnumerationRequiredStringEncoder(schema, a)
    case schema: Primitive.Required.Writer[A]   => PrimitiveRequiredStringEncoder(schema, a)
    case schema: Union.Value.Required.Writer[A] => UnionValueRequiredStringEncoder(schema, a)
