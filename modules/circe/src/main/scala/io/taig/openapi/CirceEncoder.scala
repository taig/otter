package io.taig.openapi

import io.circe.Json
import io.taig.openapi.schema.{Encoder, Primitive, Type}

import scala.annotation.tailrec

object CirceEncoder:
  val primitive: Encoder[Primitive, Json] = new Encoder[Primitive, Json]:
    def encode[B](tpe: Type[B], b: B): Json = tpe match
      case Type.BigDecimal => Json.fromBigDecimal(b)
      case Type.BigInt     => Json.fromBigInt(b)
      case Type.Boolean    => Json.fromBoolean(b)
      case Type.Double     => Json.fromDoubleOrString(b)
      case Type.Float      => Json.fromFloatOrString(b)
      case Type.Int        => Json.fromInt(b)
      case Type.Long       => Json.fromLong(b)
      case Type.String     => Json.fromString(b)

    @tailrec
    override def encode[B](fb: Primitive[B], b: B): Json = fb match
      case Primitive.Root(_, _, _, tpe)        => encode(tpe, b)
      case Primitive.Validate(primitive, _, g) => encode(primitive, g(b))