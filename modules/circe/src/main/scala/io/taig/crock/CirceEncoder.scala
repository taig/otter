package io.taig.crock

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.crock.schema.*

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder[Schema, Json]:
    override def encode[B](schema: Schema[B], b: B): Json = schema match
      case schema: Schema.Value[B] => value.encode(schema, b)
      case schema: Collection[B]   => collection.encode(schema, b)

  val value: Encoder[Schema.Value, Json] = new Encoder[Schema.Value, Json]:
    override def encode[B](schema: Schema.Value[B], b: B): Json = schema match
      case schema: Primitive[B]   => primitive.encode(schema, b)
      case schema: Enumeration[B] => enumeration.encode(schema, b)

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

    override def encode[B](primitive: Primitive[B], b: B): Json = primitive match
      case Primitive.Root(_, tpe)         => encode(tpe, b)
      case Primitive.Validate(self, _, g) => encode(self, g(b))
      case Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))

  val collection: Encoder[Collection, Json] = new Encoder[Collection, Json]:
    override def encode[B](collection: Collection[B], b: B): Json = collection match
      case Collection.Root(of, _)                   => Json.fromValues(b.map(CirceEncoder.schema.encode(of.value, _)))
      case collection: Collection.Validate[?, ?, ?] => encode(collection.self, collection.g(b))
      case collection: Collection.Optional[?, ?]    => b.fold(Json.Null)(encode(collection.self, _))

  val enumeration: Encoder[Enumeration, Json] = new Encoder[Enumeration, Json]:
    override def encode[B](enumeration: Enumeration[B], b: B): Json = enumeration match
      case Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(b))
      case Enumeration.Validate(self, _, g)     => encode(self, g(b))
      case Enumeration.Optional(self)           => b.fold(Json.Null)(encode(self, _))

  val product: Encoder[Product, Json] = new Encoder[Product, Json]:
    override def encode[B](product: Product[B], b: B): Json =
      encodeValues(product, b).map(_.toList).fold(Json.Null)(Json.fromValues)

    def encodeValues[B](product: Product[B], b: B): Option[Chain[Json]] = product match
      case Product.Empty(_)             => Chain.empty.some
      case Product.One(schema, _)       => Chain.one(CirceEncoder.schema.encode(schema.value, b)).some
      case Product.Zip(left, right, _)  => encodeValues(left, b._1) |+| encodeValues(right, b._2)
      case Product.Validate(self, _, g) => encodeValues(self, g(b))
      case Product.Optional(self)       => b.flatMap(encodeValues(self, _))
