package io.taig.otter

object StringBranchPrinter:
  def apply[A](branch: Branch[Data.Primitive, A], a: A): String = branch match
    case Branch.Modify(self, f, g) => StringBranchPrinter(branch = self, g(a))
    case Branch.Root(_, codec, _)  => StringCodecPrinter(codec = codec.value, a)
