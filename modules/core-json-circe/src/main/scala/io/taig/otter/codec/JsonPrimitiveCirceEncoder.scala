package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Primitive

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
object JsonPrimitiveCirceEncoder extends Encoder[Json.Primitive, CirceJson]:
  override def encode[W](json: Json.Primitive[W, Any], w: W): CirceJson = (json: @unchecked) match
    case json: Json.Primitive.Boolean[W, ?] => encode(json.self.self, w)
    case json: Json.Primitive.Number[W, ?]  => encode(json.self.self, w)
    case json: Json.Primitive.Text[W, ?]    => encode(json.self.self, w)

  def encode[W](schema: Primitive[W, Any], w: W): CirceJson = (schema: @unchecked) match
    case schema: Primitive.Modify[?, ?, W, ?]         => encode(schema.self, schema.g(w))
    case schema: Primitive.Boolean.Modify[?, ?, W, ?] => encode(schema.self, schema.g(w))
    case Primitive.Boolean.Root                       => CirceJson.fromBoolean(w.asInstanceOf[Boolean])
    case Primitive.Number.BigDecimal(_)               => CirceJson.fromBigDecimal(w.asInstanceOf[JBigDecimal])
    case Primitive.Number.BigInteger(_)               => CirceJson.fromBigInt(w.asInstanceOf[JBigInteger])
    case Primitive.Number.Double(_)                   => CirceJson.fromDoubleOrString(w.asInstanceOf[Double])
    case Primitive.Number.Float(_)                    => CirceJson.fromFloatOrString(w.asInstanceOf[Float])
    case Primitive.Number.Int(_)                      => CirceJson.fromInt(w.asInstanceOf[Int])
    case Primitive.Number.Long(_)                     => CirceJson.fromLong(w.asInstanceOf[Long])
    case schema: Primitive.Number.Modify[?, ?, W, ?]  => encode(schema.self, schema.g(w))
    case schema: Primitive.Text.Codec[W, ?]           => CirceJson.fromString(schema.print(w))
    case schema: Primitive.Text.Modify[?, ?, W, ?]    => encode(schema.self, schema.g(w))
    case Primitive.Text.Root(_)                       => CirceJson.fromString(w.asInstanceOf[String])
