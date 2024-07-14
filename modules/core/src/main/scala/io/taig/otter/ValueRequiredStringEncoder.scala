package io.taig.otter

import io.taig.otter as Base

object ValueRequiredStringEncoder:
  def apply[A](schema: Value.Required.Via[String, A], a: A): String = schema match
    case schema: Enumeration.Required.Via[String, A] => EnumerationRequiredStringEncoder(schema, a)
    case schema: Primitive.Required[A]               => PrimitiveRequiredStringEncoder(schema, a)
    case schema: Union.Value.Required.Via[String, A] => UnionValueRequiredStringEncoder(schema, a)
