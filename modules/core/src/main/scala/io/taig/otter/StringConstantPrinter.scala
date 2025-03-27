package io.taig.otter

object StringConstantPrinter:
  def apply[A](codec: Constant[?, A], value: A): String = codec match
    case Constant.Root(codec, value, _) => StringCodecPrinter(codec.value, value)
    case Constant.Modify(self, _, g)    => StringCodecPrinter(self, g(value))
