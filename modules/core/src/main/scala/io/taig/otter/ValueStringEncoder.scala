// package io.taig.otter

// object ValueStringEncoder:
//   def apply[A](schema: Value[?, A], a: A): Option[String] = schema match
//     case schema: Primitive[A]      => PrimitiveStringEncoder(schema, a)
//     case schema: Union.Value[?, A] => UnionValueStringEncoder(schema, a)
//     case schema: Enumeration[?, A] => EnumerationStringEncoder(schema, a)
