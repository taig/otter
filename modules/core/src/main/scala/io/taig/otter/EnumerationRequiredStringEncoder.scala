package io.taig.otter

object EnumerationRequiredStringEncoder:
  def apply[A](schema: Enumeration.Required.Writer.Via[String, A], a: A): String = schema match
    case Enumeration.Required.Transform(schema, _, f)     => transform(schema, f, a)
    case Enumeration.Required.Writer.Root(_, schema, f)   => root(schema, f, a)
    case Enumeration.Required.Writer.Transform(schema, f) => transform(schema, f, a)

  def root[A, B](schema: Value.Required.Writer.Via[String, A], f: B => A, b: B): String =
    ValueRequiredStringEncoder(schema, f(b))

  def transform[A, B](self: Enumeration.Required.Writer.Via[String, A], f: B => A, b: B): String =
    ValueRequiredStringEncoder(self, f(b))
