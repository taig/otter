package io.taig.otter

object PrimitiveRequiredStringEncoder:
  def apply[A](schema: Primitive.Required.Writer[A], a: A): String = schema match
    case Primitive.Required.Writer.Transform(self, f) => transform(self, f, a)
    case Primitive.Required.Transform(self, _, f)     => transform(self, f, a)
    case Primitive.Required.Root(tpe)                 => TypeStringEncoder(tpe, a)

  def transform[A, B](self: Primitive.Required.Writer[A], f: B => A, b: B): String =
    PrimitiveRequiredStringEncoder(self, f(b))
