package io.taig.otter

import cats.syntax.all.*

object PrimitiveStringEncoder:
  def apply[A](schema: Primitive[A], a: A): Option[String] = schema match
    case schema: Primitive.Required[A]   => PrimitiveRequiredStringEncoder(schema, a).some
    case Primitive.Optional(self)        => a.flatMap(PrimitiveStringEncoder(self, _))
    case Primitive.Transform(self, _, f) => PrimitiveStringEncoder(self, f(a))
