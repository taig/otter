package io.taig.otter

import io.circe.Json

object CirceJsonCodecEncoder:
  def apply[A](codec: Codec[?, A], a: A): Json = codec match
    case codec: Collection[?, A]  => CirceJsonCollectionEncoder(codec, a)
    case codec: Constant[?, A]    => CirceJsonConstantEncoder(codec, a)
    case codec: Dictionary[?, A]  => Json.fromFields(CirceJsonDictionaryEncoder(codec, a))
    case codec: Enumeration[?, A] => CirceJsonEnumerationEncoder(codec, a)
    case codec: Optional[?, A]    => CirceJsonOptionalEncoder(codec, a)
    case codec: Primitive[?, A]   => CirceJsonPrimitiveEncoder(codec, a)
    case codec: Record[?, A]      => Json.fromFields(CirceJsonRecordEncoder(codec, a))
    case codec: Tuple[?, A]       => Json.fromValues(CirceJsonTupleEncoder(codec, a))
    case codec: Union[?, ?, A]    => CirceJsonUnionEncoder(codec, a)
