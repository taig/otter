package io.taig.crock

import cats.data.Chain
import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.taig.crock.schema.*

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder[Schema, Json]:
    override def encode[A](schema: Schema[A], b: A): Json = schema match
      case schema: Schema.Value[A] => value.encode(schema, b)
      case schema: Collection[A]   => collection.encode(schema, b)
      case schema: Record[?, A]    => Json.fromJsonObject(record.encode(schema, b))
      case schema: Product[A]      => product.encode(schema, b)

  val value: Encoder[Schema.Value, Json] = new Encoder[Schema.Value, Json]:
    override def encode[A](schema: Schema.Value[A], b: A): Json = schema match
      case schema: Primitive[A]   => primitive.encode(schema, b)
      case schema: Enumeration[A] => enumeration.encode(schema, b)

  val primitive: Encoder[Primitive, Json] = new Encoder[Primitive, Json]:
    def encode[A](tpe: Type[A], b: A): Json = tpe match
      case Type.BigDecimal => Json.fromBigDecimal(b)
      case Type.BigInt     => Json.fromBigInt(b)
      case Type.Boolean    => Json.fromBoolean(b)
      case Type.Double     => Json.fromDoubleOrString(b)
      case Type.Float      => Json.fromFloatOrString(b)
      case Type.Int        => Json.fromInt(b)
      case Type.Long       => Json.fromLong(b)
      case Type.String     => Json.fromString(b)

    override def encode[A](primitive: Primitive[A], b: A): Json = primitive match
      case Primitive.Root(_, tpe)         => encode(tpe, b)
      case Primitive.Validate(self, _, g) => encode(self, g(b))
      case Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))

  val collection: Encoder[Collection, Json] = new Encoder[Collection, Json]:
    override def encode[A](collection: Collection[A], b: A): Json = collection match
      case Collection.Root(of, _)                   => Json.fromValues(b.map(CirceEncoder.schema.encode(of.value, _)))
      case collection: Collection.Validate[?, ?, ?] => encode(collection.self, collection.g(b))
      case collection: Collection.Optional[?, ?]    => b.fold(Json.Null)(encode(collection.self, _))

  val enumeration: Encoder[Enumeration, Json] = new Encoder[Enumeration, Json]:
    override def encode[A](enumeration: Enumeration[A], b: A): Json = enumeration match
      case Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(b))
      case Enumeration.Validate(self, _, g)     => encode(self, g(b))
      case Enumeration.Optional(self)           => b.fold(Json.Null)(encode(self, _))

  def record[A]: Encoder[Record[A, *], JsonObject] = new Encoder[Record[A, *], JsonObject]:
    override def encode[B](record: Record[A, B], b: B): JsonObject = record match
      case Record.Empty(properties) => ???
      case Record.One(field, properties) =>
        JsonObject.singleton.tupled(encodeField(field, b))
      case Record.Zip(left, right, properties)    => ???
      case Record.Validate(record, validation, g) => ???
      case Record.Optional(self)                  => ???

    def encodeField[B](field: Field[A, B], b: B): (String, Json) =
      val key = field.name(StringEncoder.value)
      val value = CirceEncoder.schema.encode(field.schema.value, b)
      (key.orEmpty, value)

  val product: Encoder[Product, Json] = new Encoder[Product, Json]:
    override def encode[A](product: Product[A], b: A): Json =
      encodeValues(product, b).map(_.toList).fold(Json.Null)(Json.fromValues)

    def encodeValues[A](product: Product[A], b: A): Option[Chain[Json]] = product match
      case Product.Empty(_)             => Chain.empty.some
      case Product.One(schema, _)       => Chain.one(CirceEncoder.schema.encode(schema.value, b)).some
      case Product.Zip(left, right, _)  => encodeValues(left, b._1) |+| encodeValues(right, b._2)
      case Product.Validate(self, _, g) => encodeValues(self, g(b))
      case Product.Optional(self)       => b.flatMap(encodeValues(self, _))
