package io.taig.otter.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Json, JsonObject}
import io.taig.otter.schema.*
import io.taig.otter.schema.Schema.AnyValue

import scala.collection.mutable.ListBuffer

object CirceEncoder:
  val schema: Encoder[Schema, Option[Json]] = new Encoder:
    override def encode[A](schema: Schema[A], a: A): Option[Json] = schema match
      case schema: Schema.Value[A] => value.encode(schema, a)
      case schema: Collection[A]   => collection.encode(schema, a).map(values => Json.fromValues(values.toList))
      case schema: Dictionary[A]   => dictionary.encode(schema, a).map(Json.fromJsonObject)
      case schema: Record[A]       => record.encode(schema, a).map(Json.fromJsonObject)
      case schema: Product[A]      => product.encode(schema, a).map(Json.fromValues)
      case schema: Coproduct[A]    => coproduct.encode(schema, a)
      case schema: AnyValue[A]     => anyValue.encode(schema, a)

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
      case Schema.Primitive.Root(_, tpe)         => encode(tpe, a).some
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(a))
      case Schema.Primitive.Optional(self)       => a.flatMap(encode(self, _))

  val collection: Encoder[Collection, Option[Chain[Json]]] = new Encoder:
    override def encode[A](collection: Collection[A], a: A): Option[Chain[Json]] = collection match
      case Schema.Collection.Root(schema, _) =>
        a.map(CirceEncoder.schema.encode(schema.value, _).getOrElse(Json.Null)).some
      case Schema.Collection.Validate(self, _, g) => encode(self, g(a))
      case Schema.Collection.Optional(self)       => a.flatMap(encode(self, _))

  val dictionary: Encoder[Dictionary, Option[JsonObject]] = new Encoder:
    override def encode[A](dictionary: Dictionary[A], a: A): Option[JsonObject] = dictionary match
      case Schema.Dictionary.Root(key, schema, _) =>
        val values = a.map { case (k, v) =>
          (
            StringEncoder.value.encode(key.value, k).orEmpty,
            CirceEncoder.schema.encode(schema.value, v).getOrElse(Json.Null)
          )
        }
        JsonObject.fromIterable(values).some
      case Schema.Dictionary.Validate(self, _, g) => encode(self, g(a))
      case Schema.Dictionary.Optional(self)       => a.flatMap(encode(self, _))

  val enumeration: Encoder[Enumeration, Option[Json]] = new Encoder:
    override def encode[A](enumeration: Enumeration[A], a: A): Option[Json] = enumeration match
      case Schema.Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(a))
      case Schema.Enumeration.Validate(self, _, g)     => encode(self, g(a))
      case Schema.Enumeration.Optional(self)           => a.flatMap(encode(self, _))

  val record: Encoder[Record, Option[JsonObject]] = new Encoder:
    override def encode[A](record: Record[A], a: A): Option[JsonObject] =
      val result = ListBuffer.empty[(String, Json)]
      encode(record, a, record.nulls.value, result)
      if result.isEmpty && record.isOptional then none else JsonObject.fromIterable(result).some

    def encode[A](record: Record[A], a: A, nulls: Null, result: ListBuffer[(String, Json)]): Unit = record match
      case Schema.Record.Empty(_)      => ()
      case Schema.Record.One(field, _) => encode(field, a, nulls, result)
      case Schema.Record.Zip(left, right, _) =>
        encode(left, a._1, nulls, result)
        encode(right, a._2, nulls, result)
      case Schema.Record.Validate(self, _, g) => encode(self, g(a), nulls, result)
      case Schema.Record.Optional(self)       => a.foreach(encode(self, _, nulls, result))

    def encode[A, B](field: Field[A, B], b: B, nulls: Null, result: ListBuffer[(String, Json)]): Unit =
      val hideNull = field.nulls.value match
        case None            => nulls === Null.Hide
        case Some(Null.Hide) => true
        case Some(Null.Show) => false

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
      case Schema.Product.Empty(_) => ()
      case Schema.Product.One(schema, _) =>
        result.append(CirceEncoder.schema.encode(schema.value, a).getOrElse(Json.Null))
      case Schema.Product.Zip(left, right, _) =>
        encode(left, a._1, result)
        encode(right, a._2, result)
      case Schema.Product.Validate(self, _, g) => encode(self, g(a), result)
      case Schema.Product.Optional(self)       => a.foreach(encode(self, _, result))

  val coproduct: Encoder[Coproduct, Option[Json]] = new Encoder:
    override def encode[A](coproduct: Coproduct[A], a: A): Option[Json] = coproduct match
      case Schema.Coproduct.Root(branch, _)        => encode(branch, a, coproduct.discriminator.value)
      case Schema.Coproduct.OrElse(left, right, _) => a.fold(encode(left, _), encode(right, _))
      case Schema.Coproduct.Validate(self, _, g)   => encode(self, g(a))
      case Schema.Coproduct.Optional(self)         => a.flatMap(encode(self, _))

    def encode[A, B](branch: Branch[A, B], b: B, discriminator: Discriminator): Option[Json] =
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

  val anyValue: Encoder[AnyValue, Option[Json]] = new Encoder:
    override def encode[A](anyValue: AnyValue[A], a: A): Option[Json] = anyValue match
      case AnyValue.Root                 => None
      case AnyValue.Validate(self, _, g) => encode(self, g(a))
