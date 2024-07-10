// package io.taig.otter

// import io.taig.otter.Plain.*
// import io.taig.otter as Base

// object EnumerationRequiredStringEncoder:
//   def apply[A](schema: Enumeration.Required.Writer[A], a: A): String = schema match
//     case Base.Enumeration.Required.Transform(schema, _, f)     => transform(schema, f, a)
//     case Base.Enumeration.Required.Writer.Root(schema, f)      => root(schema, f, a)
//     case Base.Enumeration.Required.Writer.Transform(schema, f) => transform(schema, f, a)

//   def root[A, B](schema: Value.Required.Writer[A], f: B => A, b: B): String =
//     ValueRequiredStringEncoder(schema, f(b))

//   def transform[A, B](self: Enumeration.Required.Writer[A], f: B => A, b: B): String =
//     ValueRequiredStringEncoder(self, f(b))
