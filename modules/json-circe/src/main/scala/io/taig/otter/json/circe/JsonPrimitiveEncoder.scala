package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Primitive
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveEncoder:
  def encode[A](schema: Primitive[A], value: A): Json = schema match
    case Primitive.Required.Root(tpe)                 => encode(tpe, value)
    case Primitive.Required.Modify(primitive, _, g)   => encode(primitive, g(value))
    case Primitive.Required.Validate(schema, _, _, g) => encode(schema, g(value))
    case Primitive.Optional.Root(primitive)           => value.map(encode(primitive, _)).getOrElse(Json.Null)
    case Primitive.Optional.Modify(primitive, _, g)   => encode(primitive, g(value))
    case Primitive.Optional.Validate(schema, _, _, g) => encode(schema, g(value))

  def encode[A](tpe: Type[A], value: A): Json = tpe match
    case Type.BigDecimal => (value: JBigDecimal).asJson
    case Type.BigInteger => (value: JBigInteger).asJson
    case Type.Boolean    => (value: Boolean).asJson
    case Type.Double     => (value: Double).asJson
    case Type.Float      => (value: Float).asJson
    case Type.Int        => (value: Int).asJson
    case Type.Long       => (value: Long).asJson
    case Type.String     => (value: String).asJson
