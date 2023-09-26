package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.ReferenceOrSelf

// parameter / header / ... : need value type aka a schema that can be serialized to string
// Currently this is indicated through inheritance by extending Schema.Value
// Now we are combining value schemas (or non-value) with orElse. How to statically ensure whether it's still a Schema.Value?
// What should be the result of `(a: Schema.Enumeration[A]) orElse (b: Schema.Primitive[B])`?
// Schema.Enumeration[A] | Schema.Primitive[B] ?
// Schema.Value[Either[A, B]]
// Schema[Schema.Enumeration[A] | Schema.Primitive[B], Either[A, B]]
// The latter appears reasonable, however this is also how we currently encode Collection[Schema.Primitive[A], Chain[A]] which means something else

def referenceOrSelf[A](schema: Schema[A]): Schema.Value[ReferenceOrSelf[A]] = enumeration
  .constant(string, "self")
  .orElse(schema)
  .imap {
    case Left(_)          => ReferenceOrSelf.Self
    case Right(reference) => ReferenceOrSelf.Reference(reference)
  } {
    case ReferenceOrSelf.Self             => Left("self")
    case ReferenceOrSelf.Reference(value) => Right(value)
  }
