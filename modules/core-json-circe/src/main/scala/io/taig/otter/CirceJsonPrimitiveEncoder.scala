package io.taig.otter

import io.circe.Json

object CirceJsonPrimitiveEncoder:
  def apply[A](codec: Primitive[?, A], a: A): Json = codec match
    case _: Primitive.BigDecimal                       => Json.fromBigDecimal(BigDecimal(a))
    case _: Primitive.BigInteger                       => Json.fromBigInt(BigInt(a))
    case _: Primitive.Boolean                          => Json.fromBoolean(a)
    case _: Primitive.Double                           => Json.fromDoubleOrString(a)
    case _: Primitive.Float                            => Json.fromFloatOrString(a)
    case _: Primitive.Int                              => Json.fromInt(a)
    case _: Primitive.Long                             => Json.fromLong(a)
    case _: Primitive.String                           => Json.fromString(a)
    case Primitive.Modify(self, _, g)                  => CirceJsonCodecEncoder(self, g(a))
    case Primitive.Parser(name, _, encode, _, _, _, _) => Json.fromString(encode(a))
