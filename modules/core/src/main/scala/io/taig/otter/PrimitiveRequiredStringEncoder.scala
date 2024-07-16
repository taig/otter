// package io.taig.otter

// object PrimitiveRequiredStringEncoder:
//   def apply[A](schema: Primitive.Required[A], a: A): String = schema match
//     case Primitive.Required.Transform(self, _, f) => PrimitiveRequiredStringEncoder(self, f(a))
//     case Primitive.Required.Root(_, tpe)          => TypeStringEncoder(tpe, a)
