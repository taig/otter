package io.taig.otter

object PrimitiveRequiredStringEncoder:
  def apply[A](schema: Primitive.Required[A], a: A): String = schema match
    case Primitive.Required.Transform(self, _, f) => transform(self, f, a)
    case Primitive.Required.Root(_, tpe)          => TypeStringEncoder(tpe, a)

  def transform[A, B](self: Primitive.Required[A], f: B => A, b: B): String =
    PrimitiveRequiredStringEncoder(self, f(b))
