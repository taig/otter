package io.taig.crock

import io.circe.Json
import io.taig.crock.schema.*

import scala.annotation.tailrec

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder[Schema, Json]:
    override def encode[B](schema: Schema[B], b: B): Json = schema match
      case schema: Primitive[B]   => primitive.encode(schema, b)
      case schema: Collection[B]  => collection.encode(schema, b)
      case schema: Enumeration[B] => enumeration.encode(schema, b)
      case _                      => ???

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
    override def encode[B](schema: Primitive[B], b: B): Json = schema match
      case Primitive.Root(_, tpe)           => encode(tpe, b)
      case Primitive.Validate(schema, _, g) => encode(schema, g(b))

  val collection: Encoder[Collection, Json] = new Encoder[Collection, Json]:
    @tailrec
    override def encode[B](schema: Collection[B], b: B): Json = schema match
      case Collection.Root(of, _)            => Json.fromValues(b.map(CirceEncoder.schema.encode(of.value, _)))
      case Collection.Validate(schema, _, g) => encode(schema, g(b))

  val enumeration: Encoder[Enumeration, Json] = new Encoder[Enumeration, Json]:
    override def encode[B](schema: Enumeration[B], b: B): Json = schema match
      case Enumeration.Root(mapping, schema, _)    => CirceEncoder.schema.encode(schema.value, mapping.inj(b))
      case Enumeration.Validate(enumeration, _, g) => encode(enumeration, g(b))
