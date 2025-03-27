package io.taig.otter

import io.circe.Json

object CirceJsonCodecPrinter:
  def apply[A](codec: Codec[?, A], a: A): Json = codec match
    case codec: Collection[?, A]  => CirceJsonCollectionPrinter(codec, a)
    case codec: Constant[?, A]    => CirceJsonConstantPrinter(codec, a)
    case codec: Dictionary[?, A]  => Json.fromFields(CirceJsonDictionaryPrinter(codec, a))
    case codec: Enumeration[?, A] => CirceJsonEnumerationPrinter(codec, a)
    case codec: Optional[?, A]    => CirceJsonOptionalPrinter(codec, a)
    case codec: Primitive[?, A]   => CirceJsonPrimitivePrinter(codec, a)
    case codec: Record[?, A]      => Json.fromFields(CirceJsonRecordPrinter(codec, a))
    case codec: Tuple[?, A]       => Json.fromValues(CirceJsonTuplePrinter(codec, a))
    case codec: Union[?, A]       => CirceJsonUnionPrinter(codec, a)
