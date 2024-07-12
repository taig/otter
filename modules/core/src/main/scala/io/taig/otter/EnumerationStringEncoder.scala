package io.taig.otter

import cats.syntax.all.*

object EnumerationStringEncoder:
  def apply[A](schema: Enumeration.Writer.Via[String, A], a: A): Option[String] = schema match
    case schema: Enumeration.Required.Writer.Via[String, A] => EnumerationRequiredStringEncoder(schema, a).some
    case Enumeration.Optional(self)                         => optional(self, a)
    case Enumeration.Root(_, self, f)                       => ValueStringEncoder(self, f(a))
    case Enumeration.Transform(self, _, f)                  => transform(self, f, a)
    case Enumeration.Writer.Optional(self)                  => optional(self, a)
    case Enumeration.Writer.Transform(self, f)              => transform(self, f, a)

  def optional[A](self: Enumeration.Writer.Via[String, A], a: Option[A]): Option[String] =
    a.flatMap(EnumerationStringEncoder(self, _))

  def transform[A, B](self: Enumeration.Writer.Via[String, A], f: B => A, b: B): Option[String] =
    EnumerationStringEncoder(self, f(b))
