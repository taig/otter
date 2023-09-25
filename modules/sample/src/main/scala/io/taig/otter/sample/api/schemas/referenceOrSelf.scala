package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.ReferenceOrSelf

def referenceOrSelf[A](schema: Schema[A]): Schema.Value[ReferenceOrSelf[A]] =
  string.imap {
    case "self" => ReferenceOrSelf.Self
    case value  => ???
  } {
    case ReferenceOrSelf.Self             => "self"
    case ReferenceOrSelf.Reference(value) => ???
  }
