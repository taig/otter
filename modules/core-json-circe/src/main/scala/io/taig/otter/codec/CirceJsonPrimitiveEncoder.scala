package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter as Self
import io.taig.otter.shape.JsonShape.Json

import scala.annotation.tailrec

object CirceJsonPrimitiveEncoder extends Encoder[Json.Primitive, CirceJson]:
  override def encode[A](schema: Json.Primitive[A], a: A): CirceJson =
    encode(schema = schema.self.self, a)

  @tailrec
  def encode[A](schema: Self.Primitive[A], a: A): CirceJson = schema match
    case Self.Primitive.Boolean.Modify(self, _, g)  => encode(schema = self, g(a))
    case Self.Primitive.Boolean.Root                => CirceJson.fromBoolean(a)
    case Self.Primitive.Number.BigDecimal(_)        => CirceJson.fromBigDecimal(BigDecimal(a))
    case Self.Primitive.Number.BigInteger(_)        => CirceJson.fromBigInt(BigInt(a))
    case Self.Primitive.Number.Double(_)            => CirceJson.fromDoubleOrString(a)
    case Self.Primitive.Number.Float(_)             => CirceJson.fromFloatOrString(a)
    case Self.Primitive.Number.Int(_)               => CirceJson.fromInt(a)
    case Self.Primitive.Number.Long(_)              => CirceJson.fromLong(a)
    case Self.Primitive.Number.Modify(self, _, g)   => encode(schema = self, g(a))
    case Self.Primitive.String.Modify(self, _, g)   => encode(schema = self, g(a))
    case Self.Primitive.String.Parser(_, _, encode) => CirceJson.fromString(encode(a))
    case Self.Primitive.String.Root(_)              => CirceJson.fromString(a)
