package io.taig.otter.sample.api.schemas

import io.taig.otter.dsl.*
import io.taig.otter.sample.data.ReferenceOrSelf
import io.taig.otter.{Enumeration, Value}

val self: Enumeration["self"] = enumeration.constant(string, "self")

def referenceOrSelf[A](schema: Value[A]): Value[ReferenceOrSelf[A]] = self
  .orElse(schema)
  .imap {
    case Left(_)      => ReferenceOrSelf.Self
    case Right(value) => ReferenceOrSelf.Reference(value)
  } {
    case ReferenceOrSelf.Self             => Left("self")
    case ReferenceOrSelf.Reference(value) => Right(value)
  }
