package io.taig.otter

object StringConstantPrinter:
  def apply[A](codec: Constant[Data.Primitive, A], a: A): String = codec match
    case Constant.Root(codec, reference, _) => StringCodecPrinter(codec = codec.value, reference)
    case Constant.Modify(self, _, g)        => StringConstantPrinter(codec = self, g(a))
