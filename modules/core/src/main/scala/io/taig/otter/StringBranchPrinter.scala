package io.taig.otter

object StringBranchPrinter:
  def apply[A](branch: Branch[Data.Primitive, A], a: A): String =
    StringCodecPrinter(codec = branch.codec.value, a)
