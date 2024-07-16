// package io.taig.otter

// import cats.syntax.all.*

// object EnumerationStringEncoder:
//   def apply[A](schema: Enumeration[?, A], a: A): Option[String] = schema match
//     case schema: Enumeration.Required[?, A] => EnumerationRequiredStringEncoder(schema, a).some
//     case Enumeration.Optional(self)         => a.flatMap(EnumerationStringEncoder(self, _))
//     case Enumeration.Root(_, self, f)       => ValueStringEncoder(self, f(a))
//     case Enumeration.Transform(self, _, f)  => EnumerationStringEncoder(self, f(a))
