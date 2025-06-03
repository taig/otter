// package io.taig.otter.component

// import cats.Order
// import io.taig.enumeration.ext.EnumerationValues
// import io.taig.enumeration.ext.Mapping
// import io.taig.otter.operation.EnumerationSchemaInvariant

// trait EnumerationComponent[Self[_], Value[_]](using self: EnumerationSchemaInvariant[Self, Value]):
//   final def enumeration[A, B](schema: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
//     self(schema, mapping)

//   final def enumeration[A, B: Order](schema: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
//     enumeration(schema)(using Mapping.enumeration(f))
