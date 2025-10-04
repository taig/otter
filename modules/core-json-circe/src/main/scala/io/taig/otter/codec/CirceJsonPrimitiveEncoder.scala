package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Primitive

import scala.annotation.tailrec

object CirceJsonPrimitiveEncoder extends Encoder[Primitive, CirceJson]:
  @tailrec
  override def encode[A](schema: Primitive[A], a: A): CirceJson = schema match
    case Primitive.Boolean.Modify(self, _, g)  => encode(schema = self, g(a))
    case Primitive.Boolean.Root                => CirceJson.fromBoolean(a)
    case Primitive.Number.BigDecimal(_)        => CirceJson.fromBigDecimal(BigDecimal(a))
    case Primitive.Number.BigInteger(_)        => CirceJson.fromBigInt(BigInt(a))
    case Primitive.Number.Double(_)            => CirceJson.fromDoubleOrString(a)
    case Primitive.Number.Float(_)             => CirceJson.fromFloatOrString(a)
    case Primitive.Number.Int(_)               => CirceJson.fromInt(a)
    case Primitive.Number.Long(_)              => CirceJson.fromLong(a)
    case Primitive.Number.Modify(self, _, g)   => encode(schema = self, g(a))
    case Primitive.String.Modify(self, _, g)   => encode(schema = self, g(a))
    case Primitive.String.Parser(_, _, encode) => CirceJson.fromString(encode(a))
    case Primitive.String.Root(_)              => CirceJson.fromString(a)
