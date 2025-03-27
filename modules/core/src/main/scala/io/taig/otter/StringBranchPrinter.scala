package io.taig.otter

import io.taig.otter.Branch.Tagged
import io.taig.otter.Branch.Root

object StringBranchPrinter:
  def apply[A](branch: Branch[Data.Primitive, A], a: A): String = branch match
    case Branch.Tagged(_, codec, _, _) => StringCodecPrinter(codec.value, a)
    case Branch.Root(_, codec, _)      => StringCodecPrinter(codec.value, a)
