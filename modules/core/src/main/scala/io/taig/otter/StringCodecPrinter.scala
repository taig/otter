package io.taig.otter

object StringCodecPrinter:
  def apply[A](codec: Codec[Data.Primitive, A], value: A): String = codec match
    case codec: Primitive[?, ?]             => StringPrimitivePrinter(codec, value)
    case codec: Constant[Data.Primitive, ?] => StringConstantPrinter(codec, value)
    case codec: Enumeration[?, ?]           => StringEnumerationPrinter(codec, value)
    // TODO enforce tagged object
    case codec: Union.Untagged[Data.Primitive, ?]    => StringUnionPrinter(codec, value)
