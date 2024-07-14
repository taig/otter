package io.taig.otter

import io.taig.otter as Base

object ValueRequiredStringEncoder:
  def apply[A](schema: Value.Required[?, A], a: A): String = schema match
    case schema: Enumeration.Required[?, A] => EnumerationRequiredStringEncoder(schema, a)
    case schema: Primitive.Required[A]      => PrimitiveRequiredStringEncoder(schema, a)
    case schema: Union.Value.Required[?, A] => UnionValueRequiredStringEncoder(schema, a)
