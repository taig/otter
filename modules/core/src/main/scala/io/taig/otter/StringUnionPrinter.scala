package io.taig.otter

object StringUnionPrinter:
  def apply[A](codec: Union.Untagged[Data.Primitive, A], a: A): String = codec match
    case Union.Untagged.Modify(self, _, g)     => StringUnionPrinter(self, g(a))
    case Union.Untagged.Root(branch, _)        => StringBranchPrinter(branch, a)
    case Union.Untagged.OrElse(left, right, _) => a.fold(StringUnionPrinter(left, _), StringUnionPrinter(right, _))
