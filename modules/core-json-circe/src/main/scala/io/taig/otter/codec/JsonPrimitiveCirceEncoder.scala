package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Primitive

import scala.annotation.tailrec

object JsonPrimitiveCirceEncoder extends Encoder[Json.Primitive.Write, CirceJson]:
  override def encode[A](schema: Json.Primitive.Write[A], a: A): CirceJson =
    encode(schema = schema.self.self, a)

  @tailrec
  def encode[A](schema: Primitive.Write[A], a: A): CirceJson = schema match
    case Primitive.Boolean.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Boolean.Root                  => CirceJson.fromBoolean(a)
    case Primitive.Boolean.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Number.BigDecimal(_)          => CirceJson.fromBigDecimal(BigDecimal(a))
    case Primitive.Number.BigInteger(_)          => CirceJson.fromBigInt(BigInt(a))
    case Primitive.Number.Double(_)              => CirceJson.fromDoubleOrString(a)
    case Primitive.Number.Float(_)               => CirceJson.fromFloatOrString(a)
    case Primitive.Number.Int(_)                 => CirceJson.fromInt(a)
    case Primitive.Number.Long(_)                => CirceJson.fromLong(a)
    case Primitive.Number.Modify(self, _, f)     => encode(schema = self, f(a))
    case Primitive.Number.Write.Modify(self, f)  => encode(schema = self, f(a))
    case Primitive.Text.Codec(_, _, print)       => CirceJson.fromString(print(a))
    case Primitive.Text.Modify(self, _, f)       => encode(schema = self, f(a))
    case Primitive.Text.Root(_)                  => CirceJson.fromString(a)
    case Primitive.Text.Write.Modify(self, f)    => encode(schema = self, f(a))
    case Primitive.Text.Write.Printer(_, print)  => CirceJson.fromString(print(a))
