// package io.taig.otter

// import io.taig.otter as Base
// import io.taig.otter.Plain.*

// object UnionValueRequiredStringEncoder:
//   def apply[A](schema: Union.Value.Required.Writer[A], a: A): String = schema match
//     case Base.Union.Value.Required.Combine(left, right)        => combine(left, right, a)
//     case Base.Union.Value.Required.Writer.Combine(left, right) => combine(left, right, a)
//     case Base.Union.Value.Required.Writer.Root(schema)         => ValueRequiredStringEncoder(schema, a)
//     case Base.Union.Value.Required.Writer.Transform(self, f)   => transform(self, f, a)
//     case Base.Union.Value.Required.Transform(self, _, f)       => transform(self, f, a)

//   def combine[A, B](
//       left: Union.Value.Required.Writer[A],
//       right: Union.Value.Required.Writer[B],
//       ab: Either[A, B]
//   ): String = ab.fold(ValueRequiredStringEncoder(left, _), ValueRequiredStringEncoder(right, _))

//   def transform[A, B](self: Union.Value.Required.Writer[A], f: B => A, b: B): String =
//     UnionValueRequiredStringEncoder(self, f(b))
