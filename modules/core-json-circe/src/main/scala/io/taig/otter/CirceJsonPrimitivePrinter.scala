package io.taig.otter

import io.circe.Json

object CirceJsonPrimitivePrinter:
  def apply[A](codec: Primitive[?, A], value: A): Json = codec match
    case _: Primitive.BigDecimal                       => Json.fromBigDecimal(BigDecimal(value))
    case _: Primitive.BigInteger                       => Json.fromBigInt(BigInt(value))
    case _: Primitive.Boolean                          => Json.fromBoolean(value)
    case _: Primitive.Double                           => Json.fromDoubleOrString(value)
    case _: Primitive.Float                            => Json.fromFloatOrString(value)
    case _: Primitive.Int                              => Json.fromInt(value)
    case _: Primitive.Long                             => Json.fromLong(value)
    case _: Primitive.String                           => Json.fromString(value)
    case Primitive.Modify(self, _, g)                  => CirceJsonCodecPrinter(self, g(value))
    case Primitive.Parser(name, _, encode, _, _, _, _) => Json.fromString(encode(value))
