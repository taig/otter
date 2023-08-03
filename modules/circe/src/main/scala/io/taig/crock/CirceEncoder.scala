package io.taig.crock

import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.circe.syntax.*
import io.taig.crock.schema.*
import io.taig.crock.schema.Coproduct.Discriminator
import io.taig.crock.schema.Field.Null
import io.taig.crock.schema.Record.Null

import scala.collection.mutable.ListBuffer

object CirceEncoder:
  val schema: Encoder[Schema, Option[Json]] = new Encoder:
    override def encode[A](schema: Schema[A], a: A): Option[Json] = schema match
      case schema: Schema.Value[A] => value.encode(schema, a)
      case schema: Collection[A]   => collection.encode(schema, a).map(Json.fromValues)
      case schema: Dictionary[A]   => dictionary.encode(schema, a).map(Json.fromJsonObject)
      case schema: Record[A]       => record.encode(schema, a).map(Json.fromJsonObject)
      case schema: Product[A]      => product.encode(schema, a).map(Json.fromValues)
      case schema: Coproduct[A]    => coproduct.encode(schema, a)

  val value: Encoder[Schema.Value, Option[Json]] = new Encoder:
    override def encode[A](schema: Schema.Value[A], a: A): Option[Json] = schema match
      case schema: Primitive[A]   => primitive.encode(schema, a)
      case schema: Enumeration[A] => enumeration.encode(schema, a)

  val primitive: Encoder[Primitive, Option[Json]] = new Encoder:
    def encode[A](tpe: Type[A], a: A): Json = tpe match
      case Type.BigDecimal => Json.fromBigDecimal(a)
      case Type.BigInt     => Json.fromBigInt(a)
      case Type.Boolean    => Json.fromBoolean(a)
      case Type.Double     => Json.fromDoubleOrString(a)
      case Type.Float      => Json.fromFloatOrString(a)
      case Type.Int        => Json.fromInt(a)
      case Type.Long       => Json.fromLong(a)
      case Type.String     => Json.fromString(a)

    override def encode[A](primitive: Primitive[A], a: A): Option[Json] = primitive match
      case Primitive.Root(_, tpe)         => encode(tpe, a).some
      case Primitive.Validate(self, _, g) => encode(self, g(a))
      case Primitive.Optional(self)       => a.flatMap(encode(self, _))

  val collection: Encoder[Collection, Option[Vector[Json]]] = new Encoder:
    override def encode[A](collection: Collection[A], a: A): Option[Vector[Json]] = collection match
      case Collection.Root(of, _) => a.map(CirceEncoder.schema.encode(of.value, _).getOrElse(Json.Null)).some
      case collection: Collection.Validate[?, ?, ?] => encode(collection.self, collection.g(a))
      case collection: Collection.Optional[?, ?]    => a.flatMap(encode(collection.self, _))

  val dictionary: Encoder[Dictionary, Option[JsonObject]] = new Encoder:
    override def encode[A](dictionary: Dictionary[A], a: A): Option[JsonObject] = dictionary match
      case Dictionary.Root(key, schema, _) =>
        val values = a.map { case (k, v) =>
          (
            StringEncoder.value.encode(key.value, k).orEmpty,
            CirceEncoder.schema.encode(schema.value, v).getOrElse(Json.Null)
          )
        }
        JsonObject.fromIterable(values).some
      case Dictionary.Validate(self, _, g) => encode(self, g(a))
      case Dictionary.Optional(self)       => a.flatMap(encode(self, _))

  val enumeration: Encoder[Enumeration, Option[Json]] = new Encoder:
    override def encode[A](enumeration: Enumeration[A], a: A): Option[Json] = enumeration match
      case Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(a))
      case Enumeration.Validate(self, _, g)     => encode(self, g(a))
      case Enumeration.Optional(self)           => a.flatMap(encode(self, _))

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

      if !(value.forall(_.isNull) && hideNull) then
        val key = StringEncoder.value.encode(field.key.value, field.name)
        result.append((key.orEmpty, value.getOrElse(Json.Null)))

  val product: Encoder[Product, Option[List[Json]]] = new Encoder:
    override def encode[A](product: Product[A], a: A): Option[List[Json]] =
      val result = ListBuffer.empty[Json]
      encode(product, a, result)
      if result.isEmpty && product.isOptional then none else result.toList.some

    def encode[A](product: Product[A], a: A, result: ListBuffer[Json]): Unit = product match
      case Product.Empty(_)       => ()
      case Product.One(schema, _) => result.append(CirceEncoder.schema.encode(schema.value, a).getOrElse(Json.Null))
      case Product.Zip(left, right, _) =>
        encode(left, a._1, result)
        encode(right, a._2, result)
      case Product.Validate(self, _, g) => encode(self, g(a), result)
      case Product.Optional(self)       => a.foreach(encode(self, _, result))

  val coproduct: Encoder[Coproduct, Option[Json]] = new Encoder:
    override def encode[A](coproduct: Coproduct[A], a: A): Option[Json] = coproduct match
      case Coproduct.Root(branch, _)        => encode(branch, a, coproduct.discriminator.value)
      case Coproduct.OrElse(left, right, _) => a.fold(encode(left, _), encode(right, _))
      case Coproduct.Validate(self, _, g)   => encode(self, g(a))
      case Coproduct.Optional(self)         => a.flatMap(encode(self, _))

    def encode[A, B](branch: Branch[A, B], b: B, discriminator: Coproduct.Discriminator): Option[Json] =
      discriminator match
        case Discriminator.Nested(identifier, value) =>
          Json
            .obj(
              identifier := CirceEncoder.value.encode(branch.key.value, branch.name),
              value := CirceEncoder.schema.encode(branch.schema.value, b)
            )
            .some
        case Discriminator.Merged(identifier) =>
          Json
            .obj(
              identifier := CirceEncoder.value.encode(branch.key.value, branch.name)
            )
            .deepMerge(CirceEncoder.schema.encode(branch.schema.value, b).getOrElse(Json.Null))
            .some
        case Discriminator.Keyed =>
          Json
            .obj(
              StringEncoder.value.encode(branch.key.value, branch.name).orEmpty :=
                CirceEncoder.schema.encode(branch.schema.value, b)
            )
            .some
        case Discriminator.None => CirceEncoder.schema.encode(branch.schema.value, b)
