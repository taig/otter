package io.taig.crock

import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.taig.crock.schema.*
import io.taig.crock.schema.Field.Null
import io.taig.crock.schema.Record.Null

import scala.collection.mutable.ListBuffer

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder:
    override def encode[A](schema: Schema[A], a: A): Json = schema match
      case schema: Schema.Value[A] => value.encode(schema, a)
      case schema: Collection[A]   => collection.encode(schema, a)
      case schema: Dictionary[A]   => dictionary.encode(schema, a).fold(Json.Null)(Json.fromJsonObject)
      case schema: Record[A]       => record.encode(schema, a).fold(Json.Null)(Json.fromJsonObject)
      case schema: Product[A]      => product.encode(schema, a).fold(Json.Null)(Json.fromValues)

  val value: Encoder[Schema.Value, Json] = new Encoder:
    override def encode[A](schema: Schema.Value[A], b: A): Json = schema match
      case schema: Primitive[A]   => primitive.encode(schema, b)
      case schema: Enumeration[A] => enumeration.encode(schema, b)

  val primitive: Encoder[Primitive, Json] = new Encoder:
    def encode[A](tpe: Type[A], a: A): Json = tpe match
      case Type.BigDecimal => Json.fromBigDecimal(a)
      case Type.BigInt     => Json.fromBigInt(a)
      case Type.Boolean    => Json.fromBoolean(a)
      case Type.Double     => Json.fromDoubleOrString(a)
      case Type.Float      => Json.fromFloatOrString(a)
      case Type.Int        => Json.fromInt(a)
      case Type.Long       => Json.fromLong(a)
      case Type.String     => Json.fromString(a)

    override def encode[A](primitive: Primitive[A], b: A): Json = primitive match
      case Primitive.Root(_, tpe)         => encode(tpe, b)
      case Primitive.Validate(self, _, g) => encode(self, g(b))
      case Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))

  val collection: Encoder[Collection, Json] = new Encoder:
    override def encode[A](collection: Collection[A], a: A): Json = collection match
      case Collection.Root(of, _)                   => Json.fromValues(a.map(CirceEncoder.schema.encode(of.value, _)))
      case collection: Collection.Validate[?, ?, ?] => encode(collection.self, collection.g(a))
      case collection: Collection.Optional[?, ?]    => a.fold(Json.Null)(encode(collection.self, _))

  val dictionary: Encoder[Dictionary, Option[JsonObject]] = new Encoder:
    override def encode[A](dictionary: Dictionary[A], a: A): Option[JsonObject] = dictionary match
      case Dictionary.Root(key, schema, _) =>
        val values = a.map { case (k, v) =>
          (StringEncoder.value.encode(key.value, k).orEmpty, CirceEncoder.schema.encode(schema.value, v))
        }
        JsonObject.fromIterable(values).some
      case Dictionary.Validate(self, _, g) => encode(self, g(a))
      case Dictionary.Optional(self)       => a.flatMap(encode(self, _))

  val enumeration: Encoder[Enumeration, Json] = new Encoder:
    override def encode[A](enumeration: Enumeration[A], a: A): Json = enumeration match
      case Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(a))
      case Enumeration.Validate(self, _, g)     => encode(self, g(a))
      case Enumeration.Optional(self)           => a.fold(Json.Null)(encode(self, _))

  val record: Encoder[Record, Option[JsonObject]] = new Encoder:
    override def encode[A](record: Record[A], a: A): Option[JsonObject] =
      val result = ListBuffer.empty[(String, Json)]
      encode(record, a, record.nulls.value, result)
      if result.isEmpty && record.isOptional then none else JsonObject.fromIterable(result).some

    def encode[A](record: Record[A], a: A, nulls: Record.Null, result: ListBuffer[(String, Json)]): Unit = record match
      case Record.Empty(_)      => ()
      case Record.One(field, _) => encode(field, a, nulls, result)
      case Record.Zip(left, right, _) =>
        encode(left, a._1, nulls, result)
        encode(right, a._2, nulls, result)
      case Record.Validate(self, _, g) => encode(self, g(a), nulls, result)
      case Record.Optional(self)       => a.foreach(encode(self, _, nulls, result))

    def encode[A, B](field: Field[A, B], b: B, nulls: Record.Null, result: ListBuffer[(String, Json)]): Unit =
      val hideNull = field.nulls.value match
        case Field.Null.Hide    => true
        case Field.Null.Inherit => nulls === Record.Null.Hide
        case Field.Null.Show    => false

      val value = CirceEncoder.schema.encode(field.schema.value, b)

      if !(value.isNull && hideNull) then
        val key = StringEncoder.value.encode(field.key.value, field.name)
        result.append((key.orEmpty, value))

  val product: Encoder[Product, Option[List[Json]]] = new Encoder:
    override def encode[A](product: Product[A], b: A): Option[List[Json]] =
      val result = ListBuffer.empty[Json]
      encode(product, b, result)
      if result.isEmpty && product.isOptional then none else result.toList.some

    def encode[A](product: Product[A], b: A, result: ListBuffer[Json]): Unit = product match
      case Product.Empty(_)       => ()
      case Product.One(schema, _) => result.append(CirceEncoder.schema.encode(schema.value, b))
      case Product.Zip(left, right, _) =>
        encode(left, b._1, result)
        encode(right, b._2, result)
      case Product.Validate(self, _, g) => encode(self, g(b), result)
      case Product.Optional(self)       => b.foreach(encode(self, _, result))
