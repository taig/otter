package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Primitive
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveEncoder:
  def encode[A](schema: Primitive[A], a: A): Json = schema match
    case Primitive.Required.Root(tpe)                 => encode(tpe, a)
    case Primitive.Required.Validate(schema, _, _, g) => encode(schema, g(a))
    case Primitive.Optional.Root(primitive)           => a.map(encode(primitive, _)).getOrElse(Json.Null)
    case Primitive.Optional.Validate(schema, _, _, g) => encode(schema, g(a))

  def encode[A](tpe: Type[A], a: A): Json = tpe match
    case Type.BigDecimal => (a: JBigDecimal).asJson
    case Type.BigInteger => (a: JBigInteger).asJson
    case Type.Boolean    => (a: Boolean).asJson
    case Type.Double     => (a: Double).asJson
    case Type.Float      => (a: Float).asJson
    case Type.Int        => (a: Int).asJson
    case Type.Long       => (a: Long).asJson
    case Type.String     => (a: String).asJson
