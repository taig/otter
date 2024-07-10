package io.taig.otter

import io.taig.otter.Plain.*
import io.taig.otter as Base

object ValueStringEncoder:
  def apply[A](schema: Value.Writer[A], a: A): Option[String] = schema match
    case schema: Primitive.Writer[A]   => PrimitiveStringEncoder(schema, a)
    case schema: Union.Value.Writer[A] => UnionValueStringEncoder(schema, a)
    case schema: Enumeration.Writer[A] => EnumerationStringEncoder(schema, a)
