package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Primitive

object CirceJsonPrimitiveEncoder extends Encoder[Primitive.Value[Json.Primitive, *], CirceJson]:
  override def encode[A](schema: Primitive.Value[Json.Primitive, A], a: A): CirceJson = schema match
    case Primitive.Value.Boolean.Root                   => CirceJson.fromBoolean(a)
    case _: Primitive.Value.Number.BigDecimal           => CirceJson.fromBigDecimal(BigDecimal(a))
    case _: Primitive.Value.Number.BigInteger           => CirceJson.fromBigInt(BigInt(a))
    case _: Primitive.Value.Number.Double               => CirceJson.fromDoubleOrString(a)
    case _: Primitive.Value.Number.Float                => CirceJson.fromFloatOrString(a)
    case _: Primitive.Value.Number.Int                  => CirceJson.fromInt(a)
    case _: Primitive.Value.Number.Long                 => CirceJson.fromLong(a)
    case _: Primitive.Value.String.Text                 => CirceJson.fromString(a)
    case Primitive.Value.Boolean.Modify(self, _, g)     => encode(schema = self, g(a))
    case Primitive.Value.String.Modify(self, _, g)      => encode(schema = self, g(a))
    case Primitive.Value.Number.Modify(self, _, g)      => encode(schema = self, g(a))
    case Primitive.Value.String.Parsed(self)            => encode(schema = self.value.self.value, a)
    case Primitive.Value.String.Parser(name, _, encode) => CirceJson.fromString(encode(a))
