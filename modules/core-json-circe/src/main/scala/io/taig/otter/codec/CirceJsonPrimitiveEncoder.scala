package io.taig.otter.codec

import io.taig.otter.codec.Encoder
import io.circe.Json as CirceJson
import io.taig.otter.Primitive

object CirceJsonPrimitiveEncoder extends Encoder[Primitive, CirceJson]:
  override def encode[A](schema: Primitive[A], a: A): CirceJson = schema match
    case _: Primitive.Boolean.Root                            => CirceJson.fromBoolean(a)
    case _: Primitive.Number.BigDecimal                       => CirceJson.fromBigDecimal(BigDecimal(a))
    case _: Primitive.Number.BigInteger                       => CirceJson.fromBigInt(BigInt(a))
    case _: Primitive.Number.Double                           => CirceJson.fromDoubleOrString(a)
    case _: Primitive.Number.Float                            => CirceJson.fromFloatOrString(a)
    case _: Primitive.Number.Int                              => CirceJson.fromInt(a)
    case _: Primitive.Number.Long                             => CirceJson.fromLong(a)
    case _: Primitive.String.Text                             => CirceJson.fromString(a)
    case Primitive.Boolean.Modify(self, _, g)                 => encode(schema = self, g(a))
    case Primitive.String.Modify(self, _, g)                  => encode(schema = self, g(a))
    case Primitive.Number.Modify(self, _, g)                  => encode(schema = self, g(a))
    case Primitive.String.Parser(name, _, encode, _, _, _, _) => CirceJson.fromString(encode(a))
