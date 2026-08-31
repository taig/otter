package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Primitive

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveCirceEncoder extends Encoder[Json.Primitive.Node, CirceJson]:
  override def encode[W](json: Json.Primitive.Node[W, Any], w: W): CirceJson = json match
    case Json.Primitive.Boolean.Schema(annotation) => encode(annotation.self, w)
    case Json.Primitive.Number.Schema(annotation)  => encode(annotation.self, w)
    case Json.Primitive.Text.Schema(annotation)    => encode(annotation.self, w)

  def encode[W](schema: Primitive[W, Any], w: W): CirceJson = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => CirceJson.fromBoolean(w)
    case Primitive.Number.BigDecimal(_)       => CirceJson.fromBigDecimal(w: JBigDecimal)
    case Primitive.Number.BigInteger(_)       => CirceJson.fromBigInt(w: JBigInteger)
    case Primitive.Number.Double(_)           => CirceJson.fromDoubleOrString(w)
    case Primitive.Number.Float(_)            => CirceJson.fromFloatOrString(w)
    case Primitive.Number.Int(_)              => CirceJson.fromInt(w)
    case Primitive.Number.Long(_)             => CirceJson.fromLong(w)
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => CirceJson.fromString(print(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => CirceJson.fromString(w)
