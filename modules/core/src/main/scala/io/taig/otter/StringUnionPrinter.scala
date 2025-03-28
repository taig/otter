package io.taig.otter

// object StringUnionPrinter:
//   def apply[A](codec: Union[Data.Primitive, A], a: A): String = codec match
//     case Union.Modify(self, _, g)     => StringUnionPrinter(self, g(a))
//     case Union.Root(branch, _)        => StringBranchPrinter(branch, a)
//     case Union.OrElse(left, right, _) => a.fold(StringUnionPrinter(left, _), StringUnionPrinter(right, _))
