package io.taig.otter.circe

import io.circe.Json
import io.taig.otter
import io.taig.otter.{Collection, Coproduct, Dictionary, Encoder, Enumeration, Primitive, Schema, StringEncoder, Type}
import io.circe.syntax.*

import scala.collection.immutable.VectorMap

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder:
    override def encode[B](schema: Schema[B], b: B): Json = schema match
      case schema: Schema.Value[?] => value.encode(schema, b)
      case _                       => ???

  val value: Encoder[Schema.Value, Json] = new Encoder:
    override def encode[B](schema: Schema.Value[B], b: B): Json = schema match
      case schema: Collection[?, ?] => collection.encode(schema, b)
      case schema: Dictionary[?]    => dictionary.encode(schema, b)
      case schema: Enumeration[?]   => ???
      case schema: Primitive[?]     => primitive.encode(schema, b)

  val collection: Encoder[Collection[?, *], Json] = new Encoder:
    override def encode[B](schema: Collection[?, B], b: B): Json = schema match
      case Collection.Root(_, _, schema) =>
        Json.fromValues(b.map(b => CirceEncoder.schema.encode(schema, b)).toList)
      case Collection.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Collection.Validate(self, _, g) => encode(self, g(b))

  val dictionary: Encoder[Dictionary, Json] = new Encoder:
    override def encode[B](schema: Dictionary[B], b: B): Json = schema match
      case Dictionary.Root(_, _, key, value) =>
        Json.fromFields(b.map { case (k, v) => StringEncoder.encode(key, k) -> CirceEncoder.schema.encode(value, v) })
      case Dictionary.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Dictionary.Validate(self, _, g) => encode(self, g(b))

  val primitive: Encoder[Primitive, Json] = new Encoder:
    override def encode[B](schema: Primitive[B], b: B): Json = schema match
      case Primitive.Root(_, _, _, tpe)   => encode(tpe, b)
      case Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Primitive.Validate(self, _, g) => encode(self, g(b))

    def encode[B](tpe: Type[B], b: B): Json = tpe match
      case Type.BigDecimal => (b: BigDecimal).asJson
      case Type.BigInt     => (b: BigInt).asJson
      case Type.Boolean    => (b: Boolean).asJson
      case Type.Double     => (b: Double).asJson
      case Type.Float      => (b: Float).asJson
      case Type.Int        => (b: Int).asJson
      case Type.Long       => (b: Long).asJson
      case Type.String     => (b: String).asJson
