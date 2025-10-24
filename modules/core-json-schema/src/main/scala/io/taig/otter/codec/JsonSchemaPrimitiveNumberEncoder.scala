package io.taig.otter.codec

import io.taig.otter.Json
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Keys.*
import cats.data.Chain
import io.taig.otter.Primitive

object JsonSchemaPrimitiveNumberEncoder extends Encoder[Json.Primitive.Number, CirceJson]:
  override def encode[A](schema: Json.Primitive.Number[A], a: A): CirceJson =
    encode(schema = schema.self.self, a)

  def encode[A](schema: Primitive.Number[A], a: A): CirceJson = schema match
    case Primitive.Number.BigDecimal(_) =>
      CirceJson.obj("type" := "number")
    case Primitive.Number.BigInteger(_) =>
      CirceJson.obj("type" := "integer")
    case Primitive.Number.Double(_)          => CirceJson.obj("type" := "number")
    case Primitive.Number.Float(_)           => CirceJson.obj("type" := "number")
    case Primitive.Number.Int(_)             => CirceJson.obj("type" := "integer")
    case Primitive.Number.Long(_)            => CirceJson.obj("type" := "integer")
    case Primitive.Number.Modify(self, _, g) => encode(schema = self, g(a))
