package io.taig.otter

object StringCodecPrinter:
  def apply[A](codec: Codec[Data.Primitive, A], value: A): String = codec match
    case codec: Primitive[?, ?]          => StringPrimitivePrinter(codec, value)
    case codec: Constant[?, ?]           => StringConstantPrinter(codec, value)
    case codec: Enumeration[?, ?]        => StringEnumerationPrinter(codec, value)
    case codec: Union[Data.Primitive, ?] => StringUnionPrinter(codec, value)
