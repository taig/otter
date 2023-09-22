package io.taig.otter.circe

import io.circe.Json
import io.taig.otter
import io.taig.otter.*
import io.circe.syntax.*

import scala.collection.immutable.VectorMap

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder:
    override def encode[B](schema: Schema[B], b: B): Json = schema match
      case schema: Schema.Value[?]     => value.encode(schema, b)
      case schema: Schema.Coproduct[?] => ???
      case schema: Schema.Record[?]    => ???
      case schema: Schema.Product[?]   => ???

  val value: Encoder[Schema.Value, Json] = new Encoder:
    override def encode[B](schema: Schema.Value[B], b: B): Json = schema match
      case schema: Schema.Collection[?, ?] => collection.encode(schema, b)
      case schema: Schema.Dictionary[?]    => dictionary.encode(schema, b)
      case schema: Schema.Enumeration[?]   => ???
      case schema: Schema.Primitive[?]     => primitive.encode(schema, b)

  val collection: Encoder[Schema.Collection[?, *], Json] = new Encoder:
    override def encode[B](schema: Schema.Collection[?, B], b: B): Json = schema match
      case Schema.Collection.Root(_, _, schema) =>
        Json.fromValues(b.map(b => CirceEncoder.schema.encode(schema, b)).toList)
      case Schema.Collection.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Schema.Collection.Validate(self, _, g) => encode(self, g(b))

  val dictionary: Encoder[Schema.Dictionary, Json] = new Encoder:
    override def encode[B](schema: Schema.Dictionary[B], b: B): Json = schema match
      case Schema.Dictionary.Root(_, _, key, value) =>
        Json.fromFields(b.map { case (k, v) => StringEncoder.encode(key, k) -> CirceEncoder.schema.encode(value, v) })
      case Schema.Dictionary.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Schema.Dictionary.Validate(self, _, g) => encode(self, g(b))

  val primitive: Encoder[Schema.Primitive, Json] = new Encoder:
    override def encode[B](schema: Schema.Primitive[B], b: B): Json = schema match
      case Schema.Primitive.Root(_, _, _, tpe)   => encode(tpe, b)
      case Schema.Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(b))

    def encode[B](tpe: Type[B], b: B): Json = tpe match
      case Type.BigDecimal => (b: BigDecimal).asJson
      case Type.BigInt     => (b: BigInt).asJson
      case Type.Boolean    => (b: Boolean).asJson
      case Type.Double     => (b: Double).asJson
      case Type.Float      => (b: Float).asJson
      case Type.Int        => (b: Int).asJson
      case Type.Long       => (b: Long).asJson
      case Type.String     => (b: String).asJson
