// package io.taig.otter

// import cats.syntax.all.*

// object PrimitiveStringEncoder:
//   def apply[A](schema: Primitive.Writer[A], a: A): Option[String] = schema match
//     case schema: Primitive.Required.Writer[A] => PrimitiveRequiredStringEncoder(schema, a).some
//     case Primitive.Optional(self)             => optional(self, a)
//     case Primitive.Transform(self, _, f)      => transform(self, f, a)
//     case Primitive.Writer.Optional(self)      => optional(self, a)
//     case Primitive.Writer.Transform(self, f)  => transform(self, f, a)

//   def optional[A](self: Primitive.Writer[A], a: Option[A]): Option[String] =
//     a.flatMap(PrimitiveStringEncoder(self, _))

//   def transform[A, B](self: Primitive.Writer[A], f: B => A, b: B): Option[String] =
//     PrimitiveStringEncoder(self, f(b))
