package io.taig.otter.sample.api.codecs

import io.taig.otter.dsl.*
import io.taig.otter.sample.data.ReferenceOrSelf
import io.taig.otter.{Enumeration, Union, Value}

val self: Enumeration.Required["self"] = enumeration.constant(string, "self")

def referenceOrSelf[A](codec: Value.Required[A]): Union.Required[ReferenceOrSelf[A]] = (
  self.singleton("self", ReferenceOrSelf.Self) :+
    codec.imap[ReferenceOrSelf.Reference[A]](ReferenceOrSelf.Reference.apply)(_.value)
).to
