package io.taig.otter

object ValueStringEncoder:
  def apply[A](schema: Value.Via[String, A], a: A): Option[String] = schema match
    case schema: Primitive[A]               => PrimitiveStringEncoder(schema, a)
    case schema: Union.Value.Via[String, A] => UnionValueStringEncoder(schema, a)
    case schema: Enumeration.Via[String, A] => EnumerationStringEncoder(schema, a)
