package io.taig.otter

object EnumerationRequiredStringEncoder:
  def apply[A](schema: Enumeration.Required.Via[String, A], a: A): String = schema match
    case Enumeration.Required.Transform(self, _, f)    => ValueRequiredStringEncoder(self, f(a))
    case Enumeration.Required.Root(_, schema, mapping) => ValueRequiredStringEncoder(schema, mapping(a))
