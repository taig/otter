package io.taig.otter

object PrimitiveStringEncoder:
  def apply[A](schema: Primitive.Required.Writer[A], a: A): String = schema match
    case Primitive.Required.Writer.Transform(self, f) => apply(self, f(a))
    case Primitive.Required.Transform(self, _, f)     => apply(self, f(a))
    case Primitive.Required.Root(tpe)                 => TypeStringEncoder(tpe, a)
