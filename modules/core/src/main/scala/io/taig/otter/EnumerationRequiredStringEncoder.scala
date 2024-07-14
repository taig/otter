package io.taig.otter

import io.taig.enumeration.ext.Mapping

object EnumerationRequiredStringEncoder:
  def apply[A](schema: Enumeration.Required.Via[String, A], a: A): String = schema match
    case Enumeration.Required.Transform(schema, _, f) => transform(schema, f, a)
    case Enumeration.Required.Root(_, schema, f)      => root(schema, f, a)

  def root[A, B](schema: Value.Required.Via[String, A], mapping: Mapping[B, A], b: B): String =
    ValueRequiredStringEncoder(schema, mapping(b))

  def transform[A, B](self: Enumeration.Required.Via[String, A], f: B => A, b: B): String =
    ValueRequiredStringEncoder(self, f(b))
